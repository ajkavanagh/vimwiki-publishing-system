{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Header where

-- header.hs -- extract the header (maybe) from a File
--
-- This module extracts the header from a filehandle, decodes it, and returns
-- the header and what's left of the file for rendering.  If no header is found,
-- then the header is Nothing and the filehandle remains at the start of the
-- file.

-- A header looks like this:
--

{-
--- sitegen
title: The page <title> value
template: default  # the template file (minus .extension) to render this page with
style: style.css   # the style sheet to apply to this page.
tags: [tag1, tag2] # tags to apply to the page
category: cat1     # The SINGLE category to apply to the page.
date: 2019-10-11   # The Date/time to apply to the page
updated: 2019-10-12 # Date/time the page was updated.
slug: some/slug-name  # The permanent slug to use for this page.
site-page: index   # Use this page as the index (see later)
author: [tinwood]  # Authors of this page.
draft: true        # The default is true; set to false to actually publish it.
publish: true      # default is false; set to true to publish it.
site: <site-identifier> # the site that this belongs to.
<maybe more>
---                # this indicates the end of the header.
-}

-- In order to decode this we'll have a PageHeader record which we decode yaml
-- to a structure

-- hide log, as we're pulling it in from co-log
import           Prelude            hiding (log)

-- to decode the Yaml header
import           Data.ByteString    (ByteString)
import qualified Data.List          as L
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TSE
import           Data.Yaml          ((.!=), (.:?))
import qualified Data.Yaml          as Y

-- for dates
import           Data.Time.Clock    (UTCTime)
import           Dates              (parseDate)

-- For Polysemy logging of things going on.
import           Colog.Polysemy     (Log, log)
import           Polysemy           (Member, Sem)


data RawPageHeader = RawPageHeader
    { _slug     :: !(Maybe String)
    , _title    :: !(Maybe String)
    , _template :: !String
    , _style    :: !(Maybe String)
    , _tags     :: ![String]
    , _category :: !(Maybe String)
    , _date     :: !(Maybe String)
    , _updated  :: !(Maybe String)
    , _sitePage :: !(Maybe String)
    , _author   :: ![String]
    , _draft    :: !Bool
    , _publish  :: !Bool
    , _siteID   :: !(Maybe String)
    } deriving (Show)


instance Y.FromJSON RawPageHeader where
    parseJSON (Y.Object v) = RawPageHeader
        <$> v .:? "slug"
        <*> v .:? "title"
        <*> v .:? "template" .!= "default"
        <*> v .:? "style"
        <*> v .:? "tags"     .!= []
        <*> v .:? "category"
        <*> v .:? "date"
        <*> v .:? "updated"
        <*> v .:? "site-page"
        <*> v .:? "author"  .!= []
        <*> v .:? "draft"   .!= True
        <*> v .:? "publish" .!= False
        <*> v .:? "site"
    parseJSON _ = error "Can't parse PageHeader from YAML/JSON"


-- The PageHeader is the resolved and fully parsed page header
data PageHeader = PageHeader
    { slug     :: !String
    , title    :: !String
    , template :: !String
    , style    :: !String
    , tags     :: ![String]
    , category :: !(Maybe String)
    , date     :: !(Maybe UTCTime)
    , updated  :: !(Maybe UTCTime)
    , sitePage :: !(Maybe String)
    , author   :: ![String]
    , draft    :: !Bool
    , publish  :: !Bool
    , siteID   :: !String
    } deriving (Show)


maybeDecodeHeader :: Member (Log String) r
                  => T.Text
                  -> Sem r (Maybe RawPageHeader, T.Text)
maybeDecodeHeader bs = do
    let (maybeHeader, remain) = maybeExtractHeaderBlock bs
    let maybeRawPH = Y.decodeEither' . TSE.encodeUtf8 <$> maybeHeader
    case maybeRawPH of
        Just (Right ph) -> return (Just ph, remain)
        Just (Left ex) -> do
            log @String $ "Error decoding yaml block: " ++ show ex
            return (Nothing, bs)
        Nothing -> return (Nothing, bs)


maybeExtractHeaderBlock :: T.Text -> (Maybe T.Text, T.Text)
maybeExtractHeaderBlock t =
    if isH
      then case findEnd (L.tail ls) of
          Just (hs, rs) -> (Just (T.unlines hs), T.unlines rs)
          Nothing       -> (Nothing, t)
      else (Nothing, t)
    where ls = T.lines t
          isH = isHeader ls


siteGenHeader :: T.Text
siteGenHeader = "--- sitegen"

siteGenBreak :: T.Text
siteGenBreak = "---"

isHeader :: [T.Text] -> Bool
isHeader [] = False
isHeader (x:_) = T.toLower (T.take (T.length siteGenHeader) x) == siteGenHeader


findEnd :: [T.Text] -> Maybe ([T.Text],[T.Text])
findEnd ls = case xs of
            []     -> Nothing
            (_:ys) -> Just (hs,ys)
  where (hs, xs) = L.break foundEnd ls
        foundEnd l = T.take (T.length siteGenBreak) l == siteGenBreak
