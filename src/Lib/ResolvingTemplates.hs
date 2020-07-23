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

import           System.FilePath     (FilePath, isRelative, joinPath,
                                      makeRelative, normalise, pathSeparator,
                                      splitPath, takeDirectory, takeFileName,
                                      (<.>), (</>))

import           Control.Monad       (forM)

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Polysemy            (Embed, Member, Sem, embed, embedToFinal,
                                      runFinal)
import           Polysemy.Error      (Error, throw)
import qualified Polysemy.Error      as PE
import           Polysemy.Reader     (Reader)
import qualified Polysemy.Reader     as PR
import           Polysemy.State      (State)

import           Effect.File         (File, FileException)
import qualified Effect.File         as EF
import           Effect.Logging      (LoggingMessage)
import qualified Effect.Logging      as EL

import           Types.Header        (SourceMetadata (..))

import           Lib.SiteGenConfig   (ConfigException, SiteGenConfig (..),
                                      getSiteGenConfig)
-- for tests -- remove when removing test code
import           Colog.Core          (logStringStderr)
import           Colog.Core.Severity (pattern D, pattern E, pattern I, Severity,
                                      pattern W)
import           Colog.Polysemy      (Log, runLogAction)
import qualified Colog.Polysemy      as CP

import           Data.Function       ((&))
import           Data.List           (dropWhile, inits, intercalate)
import           Data.List.Split     (splitOn)

import           Lib.Files           (filePathToMaybeSourceMetadata)
import qualified Lib.Header          as H
import           Lib.SiteGenState    (SiteGenReader, SiteGenState,
                                      makeSiteGenReader)
import           Types.Errors        (SiteGenError (..), mapSiteGenError)


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


-- | resolve the template name for an SourceMetadata
-- This looks at the template name and the route of the page and resolves the
-- template to that.  e.g. a route of thing/hello with a template "default" will
-- result in "thing/default".  This is so that the resolveTemplatePath will
-- attempt to find "thing/default.<ext>", then "_defaults/thing/default.<ext>"
-- and finally "_defaults/default.<ext>".
resolveTemplateNameForSM
    :: ResolvingTemplatesSemEffects r
    => H.SourceMetadata
    -> Sem r String
resolveTemplateNameForSM sm = do
    sgc <- PR.ask @SiteGenConfig
    let tBaseName = smTemplate sm
        hasPath = '/' `elem` tBaseName
        tFileName = tBaseName <.> sgcTemplateExt sgc
        sPath = H.smRoute sm
        sHasPath = '/' `elem` sPath
        dir = intercalate "/" $ init $ splitOn "/" sPath
        tName = case (hasPath, sHasPath) of
            (True, _)      -> tFileName
            (False, True)  -> _dropLeadingSlash dir </> tFileName
            (False, False) -> tFileName
    EL.logDebug $ T.pack $ " >> resolveTemplateNameforSM: template: "
          <> show tBaseName <> ", route: " <> show sPath <> " -> " <> show tName
    pure tName


-- | Drop the leading slash from the leading string
-- This is used when constructing the template path from the route of the
-- template so that it can then be re-found when it is included as part of
-- Ginger.
_dropLeadingSlash :: String -> String
_dropLeadingSlash "" = ""
_dropLeadingSlash ('/':s) = s
_dropLeadingSlash s = s


resolveTemplateName'
    :: ResolvingTemplatesSemEffects r
    => String
    -> Sem r (Maybe FilePath)
resolveTemplateName' tName = do
    --EL.logDebug $ T.pack $ "resolveTemplateName: trying to resolve :" <> show tName
    -- try using the filepath we were sent
    sgc <- PR.ask @SiteGenConfig
    let tDir = sgcTemplatesDir sgc
        tExt = sgcTemplateExt sgc
    resolveTemplatePath tDir tName
        -- if we got nothing back, try to resolve it with an extension added
        >>= maybe (resolveTemplatePath tDir (tName <.> tExt)) (pure . Just)


resolveTemplateName
    :: ResolvingTemplatesSemEffects r
    => String
    -> Sem r FilePath
resolveTemplateName tName =
    resolveTemplateName' tName >>=
        maybe (PE.throw $ EF.FileException tName "File Not found") pure


resolveTemplateNameRelative
    :: ResolvingTemplatesSemEffects r
    => String
    -> Sem r FilePath
resolveTemplateNameRelative tName = do
    tDir <- PR.asks @SiteGenConfig sgcTemplatesDir
    makeRelative tDir <$> resolveTemplateName tName


-- | resolve the actual path using the tPath and tDir.  If tDir is a
-- subdirectory of tPAth, remove it, and then check it directly, and then with
-- the _defaults in front of it. If we can't resolve it return Nothing
resolveTemplatePath
    :: ResolvingTemplatesSemEffects r
    => FilePath
    -> FilePath
    -> Sem r (Maybe FilePath)
resolveTemplatePath tDir tPath = do
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
          --EL.logDebug $ T.pack $ "Using " <> show rFileName
          pure $ Just rFileName



-- now some test code to see if it works


testResolveTemplatePathForSM
    :: ResolvingTemplatesSemEffects r
    => Sem r FilePath
testResolveTemplatePathForSM = do
    sgc <- getSiteGenConfig "./example-site/site.yaml" False
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
