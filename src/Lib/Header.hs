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
    ( HeaderContext(..)
    , SourceMetadata(..)
    , dropWithNewLine
    , emptySourceMetadata
    , findEndSiteGenHeader
    , isHeader
    , makeHeaderContextFromFileName
    , maxHeaderSize
    , maybeDecodeHeader
    , maybeExtractHeaderBlock
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

-- In order to decode this we'll have a SourceMetadata record which we decode
-- yaml to a structure

-- hide log, as we're pulling it in from co-log
import           Prelude               hiding (log)

import           System.FilePath       (FilePath, (</>))
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
import           Effect.Logging        (LoggingMessage)
import qualified Effect.Logging        as EL

import           Types.Constants       (maxHeaderSize)
import           Types.Header

-- Local imports
import           Lib.Dates             (parseDate)
import qualified Lib.SiteGenConfig     as S
import           Lib.Utils             (fixRoute, strToLower)


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
    , _params    :: !(Maybe Y.Object)
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
        <*> v .:? "params"
    parseJSON _ = error "Can't parse RawPageHeader from YAML/JSON"


emptySourceMetadata :: SourceMetadata
emptySourceMetadata = def


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

---


maybeDecodeHeader :: Members '[ Reader S.SiteGenConfig
                              , Reader HeaderContext
                              , Log LoggingMessage
                              ] r
                  => ByteString
                  -> Sem r (Maybe SourceMetadata)
maybeDecodeHeader bs = do
    let (maybeHeader, count) = maybeExtractHeaderBlock bs
    let maybeRawPH = Y.decodeEither' <$> maybeHeader
    case maybeRawPH of
        Just (Right rph) ->
            Just <$> makeSourceMetadataFromRawPageHeader rph count
        Just (Left ex) -> do
            EL.logError $ T.pack $ "Error decoding yaml block: " ++ show ex
            pure Nothing
        Nothing -> pure Nothing


-- TODO: this function probably shouldn't be here as it's mixing the concerns of
-- the SiteGenConfig and the HeaderContext.  I suspect that this function should
-- be in the HeaderContext when construction the actual HeaderContext for the
-- render and the SourceMetadata is not really needed (or at least the
-- RawPageHeader is not needed).  We should resolve these in HeaderContext
makeSourceMetadataFromRawPageHeader :: Members '[ Reader S.SiteGenConfig
                                                , Reader HeaderContext
                                                , Log LoggingMessage
                                                ] r
                                     => RawPageHeader
                                     -> Int
                                     {--> Sem r SourcePaeContext-}
                                     -> Sem r SourceMetadata
makeSourceMetadataFromRawPageHeader rph len = do
    sgc <- ask @S.SiteGenConfig
    rc  <- ask @HeaderContext
    --EL.logDebug $ T.pack $ "headerContext === " ++ show rc
    pageDate <- convertDate $ _date rph
    updatedDate <- convertDate $ _updated rph
    let defTemplate = if _indexPage rph then "index" else "default"
    pure SourceMetadata
        { smRoute           = fixRoute $ pick (assembleRoute rc <$> _route rph) (hcAutoSlug rc)
        , smAbsFilePath     = Just $ hcAbsFilePath rc
        , smRelFilePath     = Just $ hcRelFilePath rc
        , smVimWikiLinkPath = hcVimWikiLinkPath rc
        , smTitle           = pick (_title rph) (hcAutoTitle rc)
        , smTemplate        = pick (_template rph) defTemplate
        , smTags            = _tags rph
        , smCategory        = _category rph
        , smDate            = pageDate
        , smUpdated         = updatedDate
        , smIndexPage       = _indexPage rph
        , smAuthors         = _authors rph
        , smPublish         = _publish rph
        , smSiteId          = pick (_siteId rph) (S.sgcSiteId sgc)
        , smHeaderLen       = len
        , smParams          = _params rph
        }


assembleRoute :: HeaderContext -> String -> String
assembleRoute hc "" = "/"
assembleRoute hc route@(c:_)
  | c == '/'  = route
  | otherwise =
      let fp = hcRelFilePath hc
          dir = FP.takeDirectory fp
          dir' = if dir == "." then "" else dir
       in if '/' `elem` route
            then route
            else dir' </> route

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
    pure HeaderContext { hcAutoSlug=makeAutoSlug fpp
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
makeAutoSlug fpp = L.intercalate "" $ map fixRoute $ _path fpp


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
convertDate :: Member (Log LoggingMessage) r
            => Maybe String
            -> Sem r (Maybe UTCTime)
convertDate Nothing = pure Nothing
convertDate (Just s) =
    case parseDate s of
        Nothing -> do
            EL.logError $ T.pack $ "Couldn't decode date string: " ++ s
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
