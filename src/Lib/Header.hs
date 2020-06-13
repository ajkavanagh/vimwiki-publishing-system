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

module Lib.Header
    ( SourcePageContext(..)
    , VirtualPageContext(..)
    , dropWithNewLine
    , findEndSiteGenHeader
    , isHeader
    , maxHeaderSize
    , maybeExtractHeaderBlock
    , maybeDecodeHeader
    , emptySourcePageContext
    , HeaderContext(..)
    , makeHeaderContextFromFileName
    ) where

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
                   # note that the style is driven from the template!
tags: [tag1, tag2] # tags to apply to the page
category: cat1     # The SINGLE category to apply to the page.
date: 2019-10-11   # The Date/time to apply to the page
updated: 2019-10-12 # Date/time the page was updated.
route: some/route-name  # The permanent route to use for this page.
index-page: true        # Use this page as the index (see later)
authors: [tinwood]  # Authors of this page.
publish: false      # default is false; set to true to publish it.
site: <site-identifier> # the site that this belongs to.
<maybe more>
---                # this indicates the end of the header.
-}

-- In order to decode this we'll have a SourcePageContext record which we decode yaml
-- to a structure

-- hide log, as we're pulling it in from co-log
import           Prelude               hiding (log)

import           System.FilePath       (FilePath)
import qualified System.FilePath       as FP
import qualified System.Posix.Files    as SPF

-- to decode the Yaml header
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Default.Class    (Default, def)
import qualified Data.List             as L
import           Data.Maybe            (fromMaybe)
import qualified Data.Text             as T
import           Data.Text.Titlecase   (titlecase)
import           Data.Yaml             ((.!=), (.:?))
import qualified Data.Yaml             as Y

-- for dates
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- For Polysemy logging of things going on.
import           Colog.Polysemy        (Log, log)
import           Polysemy              (Member, Members, Sem)
import           Polysemy.Error        (Error)
import           Polysemy.Reader       (Reader, ask)

-- effects for Polysemy
import           Effect.File           (File, FileException (..), fileStatus)

-- Local imports
import           Lib.Dates             (parseDate)
import qualified Lib.SiteGenConfig     as S
import           Lib.Utils             (fixRoute, strToLower)


maxHeaderSize :: Int
maxHeaderSize = 100 * 20


data RawPageHeader = RawPageHeader
    { _route     :: !(Maybe String)
    , _title     :: !(Maybe String)
    , _template  :: !(Maybe String)
    , _tags      :: ![String]
    , _category  :: !(Maybe String)
    , _date      :: !(Maybe String)
    , _updated   :: !(Maybe String)
    , _indexPage :: !Bool
    , _authors   :: ![String]
    , _publish   :: !Bool
    , _siteId    :: !(Maybe String)
    } deriving (Show)


instance Y.FromJSON RawPageHeader where
    parseJSON (Y.Object v) = RawPageHeader
        <$> v .:? "route"
        <*> v .:? "title"
        <*> v .:? "template"
        <*> v .:? "tags"     .!= []
        <*> v .:? "category"
        <*> v .:? "date"
        <*> v .:? "updated"
        <*> v .:? "index-page" .!= False
        <*> v .:? "authors" .!= []
        <*> v .:? "publish" .!= False
        <*> v .:? "site"
    parseJSON _ = error "Can't parse RawPageHeader from YAML/JSON"


-- The SourcePageContext is the resolved and fully parsed page header
data SourcePageContext = SourcePageContext
    { spcRoute           :: !String
    , spcAbsFilePath     :: !FilePath
    , spcRelFilePath     :: !FilePath
    , spcVimWikiLinkPath :: !String
    , spcTitle           :: !String
    , spcTemplate        :: !String
    , spcTags            :: ![String]
    , spcCategory        :: !(Maybe String)
    , spcDate            :: !(Maybe UTCTime)
    , spcUpdated         :: !(Maybe UTCTime)
    , spcIndexPage       :: !Bool
    , spcAuthors         :: ![String]
    , spcPublish         :: !Bool
    , spcSiteId          :: !String
    , spcHeaderLen       :: !Int   -- the length of the headerblock; i.e. what to drop to get to the content.
    } deriving (Show, Eq)


-- we can't derive generically, as there's no default for Bool
instance Default SourcePageContext where
    def = SourcePageContext
        { spcRoute=def
        , spcAbsFilePath=def
        , spcRelFilePath=def
        , spcVimWikiLinkPath=def
        , spcTitle=def
        , spcTemplate=def
        , spcTags=def
        , spcCategory=def
        , spcDate=def
        , spcUpdated=def
        , spcIndexPage=False
        , spcAuthors=def
        , spcPublish=False
        , spcSiteId=def
        , spcHeaderLen=def
        }


emptySourcePageContext :: SourcePageContext
emptySourcePageContext = def SourcePageContext


{-
   The HeaderContext is build from the filename and file details.

   * The slug is the path from the site route split as directory names to the
     path of the slug.  The filename, if it contains spaces, is converted to '-'
     characters.
   * The file time is obtained from the file.
   * The filepath is the normalised to the site directory.
   * the autoTitle is obtained from the file name and path with spaces included,
     but stripping off the extension.

   It's stored as a context item purely so that it's easy to use in the code and
   can be provided to the renderer if needed.
-}
data HeaderContext = HeaderContext
    { hcAutoSlug        :: !String   -- slug created from the filepath
    , hcFileTime        :: !UTCTime  -- The time extracted from the file system
    , hcAbsFilePath     :: !FilePath -- the absolute file path for the filesystem
    , hcRelFilePath     :: !FilePath -- the relative file path to the site shc
    , hcVimWikiLinkPath :: !String   -- the path that vimwiki would use
    , hcAutoTitle       :: !String   -- a title created from the rel file path
    } deriving Show


data FilePathParts = FilePathParts
    { _fileName    :: !String
    , _vimWikiLink :: !String
    , _path        :: ![String]
    , _normalised  :: !FilePath
    } deriving Show


-- | A VirtualPage is one that has no source.  These represent things like
-- phantom index pages, category indexes and category pages, tags and tag pages.
data VirtualPageContext = VirtualPageContext
    { vpcRoute           :: !String
    , vpcVimWikiLinkPath :: !String
    , vpcTitle           :: !String
    , vpcTemplate        :: !String
    , vpcDate            :: !(Maybe UTCTime)
    , vpcUpdated         :: !(Maybe UTCTime)
    , vpcIndexPage       :: !Bool
    , vpcPublish         :: !Bool
    } deriving Show


-- we can't derive generically, as there's no default for Bool
instance Default VirtualPageContext where
    def = VirtualPageContext
        { vpcRoute=def
        , vpcVimWikiLinkPath=def
        , vpcTitle=def
        , vpcTemplate=def
        , vpcDate=def
        , vpcUpdated=def
        , vpcIndexPage=False
        , vpcPublish=True
        }


emptyVirtualPageContext :: VirtualPageContext
emptyVirtualPageContext = def VirtualPageContext

---


maybeDecodeHeader :: Members '[ Log String
                              , Reader S.SiteGenConfig
                              , Reader HeaderContext
                              ] r
                  => ByteString
                  -> Sem r (Maybe SourcePageContext)
maybeDecodeHeader bs = do
    let (maybeHeader, count) = maybeExtractHeaderBlock bs
    let maybeRawPH = Y.decodeEither' <$> maybeHeader
    case maybeRawPH of
        Just (Right rph) -> do
            ph <- makeSourcePageContextFromRawPageHeader rph count
            pure $ Just ph
        Just (Left ex) -> do
            log @String $ "Error decoding yaml block: " ++ show ex
            pure Nothing
        Nothing -> pure Nothing


-- TODO: this function probably shouldn't be here as it's mixing the concerns of
-- the SiteGenConfig and the HeaderContext.  I suspect that this function should
-- be in the HeaderContext when construction the actual HeaderContext for the render
-- and the SourcePageContext is not really needed (or at least the RawPageHeader is not
-- needed).  We should resolve these in HeaderContext
makeSourcePageContextFromRawPageHeader :: Members '[ Log String
                                                  , Reader S.SiteGenConfig
                                                  , Reader HeaderContext
                                                  ] r
                                => RawPageHeader
                                -> Int
                                -> Sem r SourcePageContext
makeSourcePageContextFromRawPageHeader rph len = do
    sgc <- ask @S.SiteGenConfig
    rc  <- ask @HeaderContext
    pageDate <- convertDate $ _date rph
    updatedDate <- convertDate $ _updated rph
    let defTemplate = if _indexPage rph then "index" else "default"
    pure SourcePageContext
        { spcRoute           = fixRoute $ pick (_route rph) (hcAutoSlug rc)
        , spcAbsFilePath     = hcAbsFilePath rc
        , spcRelFilePath     = hcRelFilePath rc
        , spcVimWikiLinkPath = hcVimWikiLinkPath rc
        , spcTitle           = pick (_title rph) (hcAutoTitle rc)
        , spcTemplate        = pick (_template rph) defTemplate
        , spcTags            = _tags rph
        , spcCategory        = _category rph
        , spcDate            = pageDate
        , spcUpdated         = updatedDate
        , spcIndexPage       = _indexPage rph
        , spcAuthors         = _authors rph
        , spcPublish         = _publish rph
        , spcSiteId          = pick (_siteId rph) (S.sgcSiteId sgc)
        , spcHeaderLen       = len
        }


makeHeaderContextFromFileName
    :: Members '[ File
                , Error FileException
                ] r
    => FilePath         -- the source directory of the files (absolute)
    -> FilePath         -- the filepath  (abs) of the file
    -> Sem r HeaderContext
makeHeaderContextFromFileName sfp afp = do
    fs <- fileStatus afp
    let rfp = FP.makeRelative sfp afp
    let fpp = decodeFilePath rfp
        time = posixSecondsToUTCTime $ SPF.modificationTimeHiRes fs
    pure $ HeaderContext { hcAutoSlug=makeAutoSlug fpp
                         , hcFileTime=time
                         , hcAbsFilePath=afp
                         , hcRelFilePath=_normalised fpp
                         , hcVimWikiLinkPath=_vimWikiLink fpp
                         , hcAutoTitle = makeAutoTitle fpp
                         }


-- decode the relative filepath into parts
decodeFilePath :: FilePath -> FilePathParts
decodeFilePath fp =
    let _normalised = FP.normalise fp
        noExt = FP.dropExtensions fp
        parts = FP.splitDirectories noExt
     in FilePathParts { _fileName = FP.takeFileName _normalised
                      , _vimWikiLink = strToLower noExt   -- file names are lowered
                      , _path = parts
                      , _normalised = _normalised
                      }


-- | Make a slug from the file path parts, ensuring it is lower case and spaces
-- and underscores are replaced by '-'s
makeAutoSlug :: FilePathParts -> String
makeAutoSlug fpp = FP.joinPath $ map fixRoute $ _path fpp


makeAutoTitle :: FilePathParts -> String
makeAutoTitle fpp =
    let tParts = map titlecase $ _path fpp
     in L.intercalate " / " tParts


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
                Nothing          -> (Nothing, 0)
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
         _  -> (BS.tail after, BS.length before +1)  -- add in the newline if there's more


-- Find the end site header and drop to the newline and return the block
-- text and the count of the block to that end bit.
findEndSiteGenHeader :: ByteString -> Maybe (ByteString, Int)
findEndSiteGenHeader bs =
    let (block, remain) = BS.breakSubstring siteGenBreak bs
     in case remain of
         "" -> Nothing
         _ -> let (_, count) = dropWithNewLine (BS.tail remain)
               in Just (block, BS.length block + count +1)
