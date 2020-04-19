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

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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
route: some/route-name  # The permanent route to use for this page.
site-page: index   # Use this page as the index (see later)
authors: [tinwood]  # Authors of this page.
publish: false      # default is false; set to true to publish it.
site: <site-identifier> # the site that this belongs to.
<maybe more>
---                # this indicates the end of the header.
-}

-- In order to decode this we'll have a SourcePageHeader record which we decode yaml
-- to a structure

-- hide log, as we're pulling it in from co-log
import           Prelude            hiding (log)

-- to decode the Yaml header
import           Data.ByteString    (ByteString)
import qualified Data.ByteString as BS
import qualified Data.List          as L
import           Data.Yaml          ((.!=), (.:?))
import qualified Data.Yaml          as Y
import           Data.Maybe         (fromMaybe)

-- for dates
import           Data.Time.Clock    (UTCTime)

-- For Polysemy logging of things going on.
import           Colog.Polysemy     (Log, log)
import           Polysemy           (Member, Members, Sem)
import           Polysemy.Reader    (Reader, ask)

-- Local impots
import qualified SiteGenConfig as   S
import qualified RouteContext   as   R
import           Dates              (parseDate)


maxHeaderSize :: Int
maxHeaderSize = 100 * 20


data RawPageHeader = RawPageHeader
    { _route     :: !(Maybe String)
    , _title    :: !(Maybe String)
    , _template :: !String
    , _style    :: !(Maybe String)
    , _tags     :: ![String]
    , _category :: !(Maybe String)
    , _date     :: !(Maybe String)
    , _updated  :: !(Maybe String)
    , _sitePage :: !(Maybe String)
    , _authors  :: ![String]
    , _publish  :: !Bool
    , _siteID   :: !(Maybe String)
    } deriving (Show)


instance Y.FromJSON RawPageHeader where
    parseJSON (Y.Object v) = RawPageHeader
        <$> v .:? "route"
        <*> v .:? "title"
        <*> v .:? "template" .!= "default"
        <*> v .:? "style"
        <*> v .:? "tags"     .!= []
        <*> v .:? "category"
        <*> v .:? "date"
        <*> v .:? "updated"
        <*> v .:? "site-page"
        <*> v .:? "authors" .!= []
        <*> v .:? "publish" .!= False
        <*> v .:? "site"
    parseJSON _ = error "Can't parse RawPageHeader from YAML/JSON"


-- The SourcePageHeader is the resolved and fully parsed page header
data SourcePageHeader = SourcePageHeader
    { phRoute     :: !String
    , phFileName  :: !FilePath
    , phTitle     :: !String
    , phTemplate  :: !String
    , phStyle     :: !String
    , phTags      :: ![String]
    , phCategory  :: !(Maybe String)
    , phDate      :: !(Maybe UTCTime)
    , phUpdated   :: !(Maybe UTCTime)
    , phSitePage  :: !(Maybe String)
    , phAuthors   :: ![String]
    , phPublish   :: !Bool
    , phSiteID    :: !String
    , phHeaderLen :: !Int   -- the length of the headerblock; i.e. what to drop to get to the content.
    } deriving (Show, Eq)


maybeDecodeHeader :: Members '[ Log String
                              , Reader S.SiteGenConfig
                              , Reader R.RouteContext
                              ] r
                  => ByteString
                  -> Sem r (Maybe SourcePageHeader)
maybeDecodeHeader bs = do
    let (maybeHeader, count) = maybeExtractHeaderBlock bs
    let maybeRawPH = Y.decodeEither' <$> maybeHeader
    case maybeRawPH of
        Just (Right rph) -> do
            ph <- makeSourcePageHeaderFromRawPageHeader rph count
            pure $ Just ph
        Just (Left ex) -> do
            log @String $ "Error decoding yaml block: " ++ show ex
            pure Nothing
        Nothing -> pure Nothing


-- TODO: this function probably shouldn't be here as it's mixing the concerns of
-- the SiteGenConfig and the RouteContext.  I suspect that this function should
-- be in the RouteContext when construction the actual RouteContext for the render
-- and the SourcePageHeader is not really needed (or at least the RawPageHeader is not
-- needed).  We should resolve these in RouteContext
makeSourcePageHeaderFromRawPageHeader :: Members '[ Log String
                                                  , Reader S.SiteGenConfig
                                                  , Reader R.RouteContext
                                                  ] r
                                => RawPageHeader
                                -> Int
                                -> Sem r SourcePageHeader
makeSourcePageHeaderFromRawPageHeader rph len = do
    sgc <- ask @S.SiteGenConfig
    rc  <- ask @R.RouteContext
    pageDate <- convertDate $ _date rph
    updatedDate <- convertDate $ _updated rph
    pure SourcePageHeader
        { phRoute     = pick (_route rph) (R.rcAutoSlug rc)
        , phFileName  = R.rcFileName rc
        , phTitle     = pick (_title rph) (R.rcAutoTitle rc)
        , phTemplate  = _template rph
        , phStyle     = pick (_style rph) (S.sgcDefaultStyle sgc)
        , phTags      = _tags rph
        , phCategory  = _category rph
        , phDate      = pageDate
        , phUpdated   = updatedDate
        , phSitePage  = _sitePage rph
        , phAuthors   = _authors rph
        , phPublish   = _publish rph
        , phSiteID    = pick (_siteID rph) (S.sgcSiteID sgc)
        , phHeaderLen = len
        }


-- flipped fromMaybe as pick, as it makes more sense to pick the Maybe first in
-- this scenario above
pick :: Maybe a -> a -> a
pick = flip fromMaybe


-- Convert the MaybeString into a Maybe UTCTime; logging if the date can't be
-- decoded.
convertDate :: Member (Log String) r
            => Maybe String
            -> Sem r (Maybe UTCTime)
convertDate Nothing = pure Nothing
convertDate (Just s) =
    case parseDate s of
        Nothing -> do
            log @String $ "Couldn't decode date string: " ++ s
            pure Nothing
        Just t -> pure $ Just t


-- try to extract the headerblock from the start of the file.  it looks for the
-- header using @isHeader@ and searches for the end using @findEnd@.  It returns
-- a tuple of the block of text that it found and the number of text chars that
-- make up the block (including the newline).
maybeExtractHeaderBlock :: ByteString -> (Maybe ByteString, Int)
maybeExtractHeaderBlock t =
    if isHeader t
      then let (remain, count) = dropWithNewLine t
            in case findEndSiteGenHeader remain of
                Just (header, l) -> (Just header, l + count)
                Nothing -> (Nothing, 0)
      else (Nothing, 0)


siteGenHeader :: ByteString
siteGenHeader = "--- sitegen"


-- the end of the block; note that it includes the newline BEFORE the last (at
-- least 3) hyphens
siteGenBreak :: ByteString
siteGenBreak = "\n---"


isHeader :: ByteString -> Bool
isHeader bs = BS.take (BS.length siteGenHeader) bs == siteGenHeader


-- Drop the siteGenHeader by search for an \n and droping until it is found
-- returning the text after the header and how much was dropped
dropWithNewLine :: ByteString -> (ByteString, Int)
dropWithNewLine bs =
    let (before, after) = BS.breakSubstring "\n" bs
     in case after of
         "" -> ("", BS.length before)    -- ignore the newline as no after
         _ -> (BS.tail after, BS.length before +1)  -- add in the newline if there's more


-- Find the end site header and drop to the newline and return the block
-- text and the count of the block to that end bit.
findEndSiteGenHeader :: ByteString -> Maybe (ByteString, Int)
findEndSiteGenHeader bs =
    let (block, remain) = BS.breakSubstring siteGenBreak bs
     in case remain of
         "" -> Nothing
         _ -> let (_, count) = dropWithNewLine (BS.tail remain)
               in Just (block, BS.length block + count +1)
