{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.Pandoc where

import           TextShow

import           Control.Applicative      ((<|>))
import           Control.Monad            (forM)

import           Data.ByteString          (ByteString)
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.List                as L
import           Data.Maybe               (fromJust, fromMaybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8', encodeUtf8)

import           Safe                     (headMay)

import           Colog.Polysemy           (Log)
import qualified Colog.Polysemy           as CP

import qualified Network.URI              as NU

import           Polysemy                 (Member, Sem)
import           Polysemy.Error           (Error)
import qualified Polysemy.Error           as PE
import           Polysemy.Reader          (Reader)
import qualified Polysemy.Reader          as PR
import           Polysemy.State           (State)

import           System.FilePath.Posix    (makeRelative, takeDirectory, (</>))

import           Text.Pandoc              (Pandoc)
import qualified Text.Pandoc              as TP
import qualified Text.Pandoc.Builder      as TPB
import qualified Text.Pandoc.Definition   as TPD
import qualified Text.Pandoc.Highlighting as TPH
import qualified Text.Pandoc.Walk         as TPW

import           Effect.Cache             (Cache)
import qualified Effect.Cache             as EC
import           Effect.File              (File)
import qualified Effect.File              as EF
import           Effect.Logging           (LoggingMessage)
import qualified Effect.Logging           as EL
import           Effect.Print             (Print)

import           Types.Errors             (SiteGenError)
import           Types.SiteGenState       (SiteGenReader, SiteGenState,
                                           siteFilePathToSM, siteVimWikiLinkMap)

import qualified Lib.Header               as H
import           Lib.SiteGenConfig        (SiteGenConfig)
import qualified Lib.SiteGenConfig        as SGC

import           Lib.PandocUtils          (convertVimWikiLinks, countWords,
                                           extractToc,
                                           pandocToContentTextEither,
                                           pandocToSummaryTextEither,
                                           parseMarkdown,
                                           renderTocItemsToHtml,
                                           stripMoreIndicator)

import           Lib.Utils                (maybeM', returnFirstMaybeResultM)


type PandocSemEffects r
  =    ( Member File r
       , Member (Cache Pandoc) r
       , Member (Cache Int) r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log LoggingMessage) r
       , Member Print r
       )


-- | process the SourceMetadata record to a pandoc AST and cache it/return it.
-- If it already exists just return the cached version.  In the process, vimwiki
-- links are converted / validated into links that will work in the target
-- website.
cachedProcessSMToPandocAST
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Sem r TP.Pandoc
cachedProcessSMToPandocAST sm = do
    let key = T.pack (H.smRoute sm) <> "-pandoc-ast"
    EC.fetch key >>= \case
        Just pd' -> pure pd'
        Nothing -> case H.smAbsFilePath sm of
            Nothing -> pure $ TPB.doc $ TPB.singleton TPD.Null
            Just absFile -> do
                EL.logInfo $ T.pack $ "Parsing and processing: " <> fromJust (H.smRelFilePath sm)
                bs <- EF.readFile absFile (Just $ H.smHeaderLen sm) Nothing
                pd <- PE.fromEither $ parseMarkdown bs
                let mDir = takeDirectory <$> H.smAbsFilePath sm
                pd' <- processPandocLinksM mDir (convertVimWikiLinks pd)
                EC.store key pd'
                pure pd'


{-processPandocASTM-}
    {-:: PandocSemEffects r-}
    {-=> TP.Pandoc-}
    {--> Sem r TP.Pandoc-}
{-processPandocASTM pd = do-}
    {-vws <- PR.asks @SiteGenReader siteVimWikiLinkMap-}
    {-pure $ processPandocAST vws pd-}


-- | process the Pandoc document to find the links.  These are then converted
-- into target links within the document.  The (optional) path passed it to
-- allow relative links to be processed.
processPandocLinksM
    :: PandocSemEffects r
    => Maybe String          -- the optional directory to root relative links to
    -> TP.Pandoc
    -> Sem r TP.Pandoc
processPandocLinksM mDir = TPW.walkM (walkLinksInInlinesM mDir)


walkLinksInInlinesM
    :: PandocSemEffects r
    => Maybe String          -- the optional directory to root relative links to
    -> [TP.Inline]
    -> Sem r [TP.Inline]
walkLinksInInlinesM mDir xs = L.concat <$> walkLinksInInlinesM' mDir [] xs


walkLinksInInlinesM'
    :: PandocSemEffects r
    => Maybe String          -- the optional directory to root relative links to
    -> [[TP.Inline]]
    -> [TP.Inline]
    -> Sem r [[TP.Inline]]
walkLinksInInlinesM' mDir ds xs = do
    let (as, bs) = L.break findLink xs
    case bs of
        [] -> pure $ L.reverse $ as : ds
        (b:bs') -> do
            b' <- maybeRewriteLinkM mDir b
            walkLinksInInlinesM' mDir (b':as:ds) bs'
  where
      findLink TPD.Link {} = True
      findLink _           = False


-- | using convertLinkM (below) maybe convert the link in the TP.Inline into
-- something that the browser can resolve; it may just get converted into text.
-- Note that it takes an TP.Inline and converts it to a list as it might have to
-- convert the Link into a Text field.
maybeRewriteLinkM
    :: PandocSemEffects r
    => Maybe String          -- the optional directory to root relative links to
    -> TP.Inline
    -> Sem r [TP.Inline]
maybeRewriteLinkM mDir link@(TPD.Link attr desc (url, title)) = do
    let url' = T.unpack url
    if NU.isRelativeReference url'
      -- can we parse it
      then case NU.parseRelativeReference url' of
          Nothing -> pure [link]   -- assume it's something else and leave it alone
          Just uri -> do
              mUri <- convertLinkM mDir url
              case mUri of
                  -- Nothing matched so convert it to text (i.e. lose the link)
                  Nothing -> pure $ if null desc
                              then TPB.toList $ TPB.text title
                              else desc
                  -- newUri is the coverted link; so we'll build that back in.
                  Just newUri -> pure [TPD.Link attr desc (newUri, title)]
      -- it was absolute or something else; thus we just ignore the link
      else pure [link]
maybeRewriteLinkM _ _ = error "Must only pass a Link to this function"


-- | convert a link that is in a Pandoc Document to a link in the target site if
-- it actually (will) exist.  i.e. it needs to point to another SourceMetadata's
-- route (permalink) or to a resource in the site (static directories).  If it
-- doesn't then return None.
--
-- To do this we may have to know which absolute path this is relative to (and
-- be in a Monad that has the site's config) as well as the link.  So the theory
-- is:
--
--  1. If the link is relative, and doesn't have an extension, we assume it is a
--     regular wikilink.  If so, rebase it to the wiki root as a page.  e.g. add
--     on the difference between sgcVimWikiRoot and sgcSource which will be the
--     offset of the source directory for the website from the root of the
--     vimwiki.
--  2. If the link is absolute, then leave it as is.
--  3. Compare the link to the SourceMetadata's siteVimWikiLink map.  If it is
--     found there, then convert the link to the target route for the page.
--     This will be determined by the SourceMetadata.
--  4. If it doesn't match a SourceMetadata page, then we need to compare it to
--     the static directories, and see if it's one of those files.  In this
--     case, if the link is relative, we convert it to the absolute for the
--     page, canonicalise it, and then match prefixs against the static-dirs.
--     If one of the dirs matches, then we see if the file actually exists.  If
--     it does, we strip the static-dir prefix and return the (now site rooted)
--     link back.
--  5. If all of this fails then we return Nothing to indicate that the link
--     won't be valid and so that the calling function can make a decision on
--     it.
convertLinkM
    :: PandocSemEffects r
    => Maybe String          -- the optional directory to root relative links to
    -> Text
    -> Sem r (Maybe Text)
convertLinkM mDir link = do
    let url' = T.unpack link
    if NU.isAbsoluteURI url'  -- this looks for scheme: before the url.  False is not "isAbsoluteURI"
      then pure $ Just link
      else
          -- it's a local link of some sort, let's parse it.
          maybeM' Nothing (NU.parseRelativeReference url') $ \nuUri ->
              maybeM' Nothing (extractLinkIfLocal nuUri) $ \link -> do
                  mPath <- returnFirstMaybeResultM link
                                                   [ matchLinkToSM mDir
                                                   , matchLinkToSiteStaticM mDir
                                                   ]
                  let mUri = (\p -> nuUri {NU.uriPath=p}) <$> mPath
                  pure $ T.pack . show <$> mUri


-- | Try to match a path (excluding the query) to a SourceMetadata page
-- Returns Nothing if the URI doesn't match a vimwiki page.
-- The path in the link will be in the form of:
--   /absolute/link-to-a-page/   (which may include spaces)
--   /absolute/link-to-a-page   (which may include spaces)
--   relative-page
--   relative-page/
--   relative/link-to-a-page
--   relative/link-to-a-page/
--
-- It also requires the uriScheme to be "" (empty) and for there to be no
-- extension on the uriPath part.  Both of these cases will result in a Nothing.
--
-- the sgcVimWikiRoot points to the root of the VimWiki.
-- the sgcSource points to the root of the files that are scanned for the SMs
-- the sgcRoot is the directory of the config file
-- the difference between the sgcVimWikiRoot and sgcSource (if there is one) is
-- the absolute prefix of the page is thus, this difference which is
-- sgcSourceRelDir
--
-- Thus, if the path is absolute then it needs to match the sgcSourceRelDir
matchLinkToSM
    :: PandocSemEffects r
    => Maybe String           -- The absolute directory of the page that they link is in.
    -> String                 -- the link to match against
    -> Sem r (Maybe String)
matchLinkToSM mDir link = do
    mAbsPath <- relativeLinkToAbsPath link mDir
    maybeM' Nothing mAbsPath $ \absPath -> do
        matchPath <- ensureIsSourceFilePathM absPath
        wikiRoot <- PR.asks @SiteGenConfig SGC.sgcVimWikiRoot
        smsMap <- PR.asks @SiteGenReader siteFilePathToSM
        let matchedSm = matchPath `HashMap.lookup` smsMap
            newPath = H.resolveLinkFor <$> matchedSm
        pure newPath


-- | match a local link to a static path file.  If the link is absolute (i.e.
-- starts with a "/") then we just add the static path to it.  If it is
-- relative, then we need to find out where in the path the SM file is, and then
-- attempt to match that.  It's not terribly useful having relative, local, link
-- (i.e. one not rooted with /) but there may be circumstances where the static
-- files are mapped into the same directories as the output files.  Note it gets
-- really wierd if the SM is re-mapped to another location.  Just use "/" rooted
-- links!
matchLinkToSiteStaticM
    :: PandocSemEffects r
    => Maybe String             -- The absolute directory of the page that they link is in.
    -> String                   -- the link to match to.
    -> Sem r (Maybe String)
matchLinkToSiteStaticM mDir link = do
    -- EL.logDebug $ T.pack $ "matchLinkToSiteStaticM matchPath is : " <> show link
    staticDirs <- PR.asks @SiteGenConfig SGC.sgcStaticDirs
    mAbsLink <- if "/" `L.isPrefixOf` link
                 then pure (Just link)
                 else maybeM' Nothing mDir $ \smDirectory -> do
                    -- this is the canonical path to the file to
                    -- the link
                    cDir <- EF.canonicalizePath (smDirectory </> link)
                    -- now strip off the wiki root dir
                    wikiRoot <- PR.asks @SiteGenConfig SGC.sgcVimWikiRoot
                    pure $ if wikiRoot `L.isPrefixOf` cDir
                    then Just $ "/" <> makeRelative wikiRoot cDir
                    else Nothing
    maybeM' Nothing mAbsLink $ \absLink -> do
        let testPaths = map (<> absLink) staticDirs
        resultPaths <- forM testPaths EF.doesFileExist
        pure $ if or resultPaths
          then Just link
          else Nothing


-- TODO: this is in the wrong file; should be in a links utility file.
-- | extract the link and return it, if the sheme indicates it's a local link.
extractLinkIfLocal :: NU.URI -> Maybe String
extractLinkIfLocal uri =
    let scheme = NU.uriScheme uri
        path = NU.uriPath uri
    in if scheme /= ""
         then Nothing
         else Just path


-- TODO: this is in the wrong file; should be in a vimwiki file?
-- | Get the absolute path to the source for the source meta assuming it is a
-- file.  Returns None if it isn't a file path.
relativeLinkToAbsPath
    :: PandocSemEffects r
    => String                   -- The link path to convert to an abs path
    -> Maybe String             -- The absolute directory of the page that they link is in.
    -> Sem r (Maybe String)
relativeLinkToAbsPath path mDir = do
    wikiRoot <- PR.asks @SiteGenConfig SGC.sgcVimWikiRoot
    let isAbsolute = "/" `L.isPrefixOf` path
    maybeM' Nothing mDir $ \dir -> do
        let matchPath = if isAbsolute
                        then wikiRoot <> path
                        else dir </> path
        cPath <- EF.canonicalizePath matchPath
        pure (Just cPath)


ensureIsSourceFilePathM
    :: PandocSemEffects r
    => String
    -> Sem r String
ensureIsSourceFilePathM path = do
    ext <- PR.asks @SiteGenConfig SGC.sgcExtension
    if ext `L.isSuffixOf` path
      then pure path
      else pure $ path <> ext


smContentM :: PandocSemEffects r => H.SourceMetadata -> Sem r Text
smContentM sm = do
    skylightStyle <- PR.asks @SiteGenConfig SGC.sgcSkylightStyle
    PE.fromEither
      =<< pandocToContentTextEither  skylightStyle . stripMoreIndicator
      <$> cachedProcessSMToPandocAST sm


smSummaryM
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Bool
    -> Sem r (Text, Bool)
smSummaryM sm isRich = do
    maxSummaryWords <- PR.asks @SiteGenConfig SGC.sgcMaxSummaryWords
    skylightStyle <- PR.asks @SiteGenConfig SGC.sgcSkylightStyle
    -- get Either err (plain, rick) as a Summary
    res <- pandocToSummaryTextEither skylightStyle maxSummaryWords <$> cachedProcessSMToPandocAST sm
    PE.fromEither $ fmap (if isRich then snd else fst) res


smTocM
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Maybe Int
    -> Sem r Text
smTocM sm mLevels = do
    skylightStyle <- PR.asks @SiteGenConfig SGC.sgcSkylightStyle
    tocItems <- extractToc <$> cachedProcessSMToPandocAST sm
    let levels = fromMaybe 6 mLevels
    PE.fromEither $ renderTocItemsToHtml skylightStyle levels tocItems


markdownToHTML :: PandocSemEffects r => Text -> Sem r Text
markdownToHTML txt = do
    pd <- PE.fromEither $ parseMarkdown (encodeUtf8 txt)
    pd' <- processPandocLinksM Nothing (convertVimWikiLinks pd)
    skylightStyle <- PR.asks @SiteGenConfig SGC.sgcSkylightStyle
    PE.fromEither $ pandocToContentTextEither skylightStyle pd'


wordCountM
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Sem r Int
wordCountM sm = do
    let key = T.pack (H.smRoute sm) <> "-wordcount"
    EC.fetch @Int key >>= \case
        Just wc -> pure wc
        Nothing -> do
            wc <- countWords <$> cachedProcessSMToPandocAST sm
            EC.store @Int key wc
            pure wc


styleToCssM
    :: PandocSemEffects r
    => Maybe Text
    -> Sem r Text
styleToCssM mStyle = do
    mStyleSite <- PR.asks @SiteGenConfig SGC.sgcSkylightStyle
    let sStyle = flip L.lookup TPH.highlightingStyles =<< (mStyle <|> mStyleSite)
    case sStyle of
        Nothing -> do
            EL.logDebug "styleToCssM: asked for style CSS but no style configured not passed to function"
            pure ""
        Just s -> pure $ T.pack $ TPH.styleToCss s
