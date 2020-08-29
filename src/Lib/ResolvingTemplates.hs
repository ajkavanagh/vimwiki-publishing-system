{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- for the pattern stuff with Colog
{-# LANGUAGE PatternSynonyms     #-}


module Lib.ResolvingTemplates where

import           System.FilePath      (FilePath, isRelative, joinPath,
                                       makeRelative, normalise, pathSeparator,
                                       splitPath, takeDirectory, takeFileName,
                                       (<.>), (</>))

import           Control.Monad        (forM, join)
import           Safe                 (headMay)

import           Data.ByteString      (ByteString)
import           Data.Maybe           (isNothing)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Polysemy             (Embed, Member, Sem, embed, embedToFinal,
                                       runFinal)
import           Polysemy.Error       (Error, throw)
import qualified Polysemy.Error       as PE
import           Polysemy.Reader      (Reader)
import qualified Polysemy.Reader      as PR
import           Polysemy.State       (State)

import           Effect.File          (File, FileException)
import qualified Effect.File          as EF
import           Effect.Logging       (LoggingMessage)
import qualified Effect.Logging       as EL

import           Types.Errors         (SiteGenError (..), mapSiteGenError)
import           Types.Header         (SourceMetadata (..))

import           Lib.SiteGenConfig    (ConfigException, SiteGenConfig (..),
                                       getSiteGenConfig)
-- for tests -- remove when removing test code
import           Colog.Core           (logStringStderr)
import           Colog.Core.Severity  (pattern D, pattern E, pattern I,
                                       Severity, pattern W)
import           Colog.Polysemy       (Log, runLogAction)
import qualified Colog.Polysemy       as CP

import           Data.Function        ((&))
import           Data.List            (dropWhile, inits, intercalate)
import           Data.List.Split      (splitOn)

import           Lib.BuiltInTemplates (textForTemplate)
import           Lib.Files            (filePathToMaybeSourceMetadata)
import qualified Lib.Header           as H
import           Lib.SiteGenState     (SiteGenReader, SiteGenState,
                                       makeSiteGenReader)


{-
   So the idea here is that we have a templates directory, and in the templates
   directory we will have (possibly) subdirectories and template files.

    ./templates/
        _templates/
            index.html.j2
            default.html.j2
        default.html.j2
        index.html.j2
        posts/
            index.html.j2

   So the idea is that in 'posts' and file that is asking for the 'default'
   template will end up with the './templates/_defaults/default.html.j2'
   templates, but if asked for the 'index' template, it would get
   './templates/posts/index.html.j2'

-}


-- The Effects needed for the functions in this module
type ResolvingTemplatesSemEffects r
    = ( Member File r
      , Member (Reader SiteGenConfig) r
      , Member (Reader SiteGenReader) r
      , Member (Error SiteGenError) r
      , Member (Error FileException) r
      , Member (Error ConfigException) r
      , Member (State SiteGenState) r
      , Member (Log LoggingMessage) r
      )


-- | resolve the template name for an SourceMetadata to a searchable template.
-- This function is used to provide the initial template name to Ginger for
-- resolving.
resolveTemplateNameForSM
    :: ResolvingTemplatesSemEffects r
    => H.SourceMetadata
    -> Sem r String
resolveTemplateNameForSM sm = do
    sgc <- PR.ask @SiteGenConfig
    let tFileName = smTemplate sm
        hasPath = '/' `elem` tFileName
        sPath = H.smRoute sm
        sHasPath = '/' `elem` sPath
        dir = intercalate "/" $ init $ splitOn "/" sPath
        tName = case (hasPath, sHasPath) of
            (True, _)      -> tFileName
            (False, True)  -> _dropLeadingSlash dir </> tFileName
            (False, False) -> tFileName
    EL.logDebug $ T.pack $ " >> resolveTemplateNameforSM: template: "
          <> show tFileName <> ", route: " <> show sPath <> " -> " <> show tName
    pure tName


-- | Drop the leading slash from the leading string
-- This is used when constructing the template path from the route of the
-- template so that it can then be re-found when it is included as part of
-- Ginger.
_dropLeadingSlash :: String -> String
_dropLeadingSlash ""      = ""
_dropLeadingSlash ('/':s) = s
_dropLeadingSlash s       = s


-- These functions are called from Ginger to resolve a template name to an
-- actual file that exists and can be loaded.

resolveTemplateName
    :: ResolvingTemplatesSemEffects r
    => String
    -> Sem r (Maybe FilePath)
resolveTemplateName tName = do
    --EL.logDebug $ T.pack $ "resolveTemplateName: trying to resolve :" <> show tName
    -- try using the filepath we were sent
    sgc <- PR.ask @SiteGenConfig
    let tDirs = sgcTemplatesDirs sgc
        tExt = sgcTemplateExt sgc
    -- try resolving with the extension added (i.e. assume that it doesn't have
    -- it)
    resolveTemplatePath tDirs tName
        -- if we got nothing back, try to resolve it without the extension added
        -- i.e. assume that it might already have it.
        >>= maybe (resolveTemplatePath tDirs (tName <.> tExt)) (pure . Just)


resolveTemplateToByteString
    :: ResolvingTemplatesSemEffects r
    => String
    -> Sem r (Maybe ByteString)
resolveTemplateToByteString tName = do
    mFp <- resolveTemplateName tName
    case mFp of
        Nothing -> do
            let mBs = textForTemplate tName
            case mBs of
                Nothing -> do
                    EL.logDebug $ T.pack $ " No builtin template for template name: " <> tName
                    pure Nothing
                Just bs -> do
                    EL.logDebug $ T.pack $ " Using builtin template for: " <> tName
                    pure $ Just bs
        Just fp -> do
            EL.logDebug $ T.pack $ "Found template name: " <> tName
            Just <$> EF.readFile fp Nothing Nothing


resolveTemplatePath
    :: ResolvingTemplatesSemEffects r
    => [FilePath]
    -> FilePath
    -> Sem r (Maybe FilePath)
resolveTemplatePath tDirs tPath = do
    mPossibles <- forM tDirs $ \tDir -> resolveTemplatePath' tDir tPath
    let cPath = join $ headMay $ dropWhile isNothing mPossibles
    {-EL.logDebug $ T.pack $ "  resolveTemplatePath: chosen template: " <> maybe "<not-found>" show cPath-}
    pure cPath


-- | resolve the actual path using the tPath and single tDir.  If tDir is a
-- subdirectory of tPAth, remove it, and then check it directly, and then with
-- the _defaults in front of it. If we can't resolve it return Nothing
resolveTemplatePath'
    :: ResolvingTemplatesSemEffects r
    => FilePath
    -> FilePath
    -> Sem r (Maybe FilePath)
resolveTemplatePath' tDir tPath = do
    --EL.logDebug $ T.pack $ "resolveTemplatePath for: " <> tPath <> " at " <> tDir
    let relPath = if isRelative tPath then tPath else makeRelative tDir tPath
        fileName = takeFileName relPath
        hasPath = pathSeparator `elem` relPath
        tryPaths = map (normalise . (tDir </>))
            $ if hasPath
                -- try actual, then with _defaults, and then _defaults/fileName
                then [relPath, "_defaults" </> relPath, "_defaults" </> fileName]
                else [relPath, "_defaults" </> relPath]
    --EL.logDebug $ T.pack $ " --- try paths: " ++ intercalate " : " tryPaths
    exists <- forM tryPaths EF.doesFileExist
    let pairs = dropWhile (not.snd) $ zip tryPaths exists
    if null pairs
      then do
          --EL.logDebug "Couldn't find a file"
          pure Nothing
      else do
          let rFileName = fst $ head pairs
          {-EL.logDebug $ T.pack $ "  resolveTemplatePath': chosen template: " <> show rFileName-}
          pure $ Just rFileName



-- now some test code to see if it works


testResolveTemplatePathForSM
    :: ResolvingTemplatesSemEffects r
    => Sem r FilePath
testResolveTemplatePathForSM = do
    sgc <- getSiteGenConfig "./example-site/site.yaml" False True
    let file = "./example-site/src/posts/test_post.md"
    fp <- EF.makeAbsolute file
    PR.runReader @SiteGenConfig sgc $ do
        mSm <- filePathToMaybeSourceMetadata fp
        case mSm of
            Nothing -> throw $ EF.FileException file "Not a sitegen file"
            (Just sm) -> do
                let sgr = makeSiteGenReader [sm]
                PR.runReader @SiteGenReader sgr
                    $ PR.runReader @SiteGenConfig sgc
                    $ resolveTemplateNameForSM sm


-- Run the Sem r monad to IO
{-runTest x = x & EF.fileToIO-}
              {-& PE.mapError @FileException mapSiteGenError-}
              {-& PE.mapError @ConfigException mapSiteGenError-}
              {-& PE.errorToIOFinal @SiteGenError-}
              {-& runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]-}
              {-& runLogAction @IO (EL.logActionLevel D)-}
              {-& embedToFinal @IO-}
              {-& runFinal @IO-}


--test = testResolveTemplatePathForSM & runTest
