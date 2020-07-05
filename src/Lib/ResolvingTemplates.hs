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


module Lib.ResolvingTemplates where

import           System.FilePath   (FilePath, isRelative, joinPath,
                                    makeRelative, normalise, pathSeparator,
                                    splitPath, takeDirectory, takeFileName,
                                    (<.>), (</>))

import           Control.Monad     (forM)

import           Polysemy          (Embed, Members, Sem, embed, embedToFinal,
                                    runFinal)
import           Polysemy.Error    (Error, throw)
import qualified Polysemy.Error    as PE
import           Polysemy.Reader   (Reader)
import qualified Polysemy.Reader   as PR

import           Effect.File       (File, FileException)
import qualified Effect.File       as EF

import           Lib.Header        (SourcePageContext (..),
                                    VirtualPageContext (..))
import           Lib.SiteGenConfig (ConfigException, SiteGenConfig (..),
                                    getSiteGenConfig)
-- for tests -- remove when removing test code
import           Colog.Core        (logStringStderr)
import           Colog.Polysemy    (Log, runLogAction)
import qualified Colog.Polysemy    as CP

import           Data.Function     ((&))
import           Data.List         (dropWhile, inits, intercalate)
import           Data.List.Split   (splitOn)

import           Lib.Errors        (SiteGenError (..), mapSiteGenError)
import           Lib.Files         (filePathToMaybeSourcePageContext)
import qualified Lib.Header        as H
import           Lib.SiteGenState  (SiteGenReader, makeSiteGenReader)


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


-- | resolve the template name for an SC
-- This looks at the template name and the route of the page and resolves the
-- template to that.  e.g. a route of thing/hello with a template "default" will
-- result in "thing/default".  This is so that the resolveTemplatePath will
-- attempt to find "thing/default.<ext>", then "_defaults/thing/default.<ext>"
-- and finally "_defaults/default.<ext>".
resolveTemplateNameForSC
    :: Members '[ Reader SiteGenReader
                , Reader SiteGenConfig
                , File
                , Error SiteGenError
                , Log String
                ] r
    => H.SourceContext
    -> Sem r String
resolveTemplateNameForSC sc = do
    sgc <- PR.ask @SiteGenConfig
    let tBaseName = case sc of
            (H.SPC spc) -> spcTemplate spc
            (H.VPC vpc) -> vpcTemplate vpc
        hasPath = '/' `elem` tBaseName
        tFileName = tBaseName <.> sgcTemplateExt sgc
        sPath = H.scRoute sc
        sHasPath = '/' `elem` sPath
        dir = intercalate "/" $ init $ splitOn "/" sPath
        tName = case (hasPath, sHasPath) of
            (True, _)      -> tFileName
            (False, True)  -> dir </> tFileName
            (False, False) -> tFileName
    CP.log $ " >> resolveTemplateNameforSC: template: "
          <> show tBaseName <> ", route: " <> show sPath <> " -> " <> show tName
    pure tName


resolveTemplateName
    :: Members '[ File
                , Error FileException
                , Reader SiteGenConfig
                , Log String
                ] r
    => String
    -> Sem r FilePath
resolveTemplateName tName = do
    CP.log @String $ "resolveTemplateName: trying to resolve :" <> show tName
    -- try using the filepath we were sent
    sgc <- PR.ask @SiteGenConfig
    let tDir = sgcTemplatesDir sgc
        tExt = sgcTemplateExt sgc
    mFp <- resolveTemplatePath tDir tName >>= (\case
        -- if we got nothing back, try to resolve it with an extension added
        Nothing -> resolveTemplatePath tDir (tName <.> tExt)
        fp@(Just _) -> pure fp)
    maybe (PE.throw $ EF.FileException tName "File Not found") pure mFp


resolveTemplateNameRelative
    :: Members '[ File
                , Error FileException
                , Reader SiteGenConfig
                , Log String
                ] r
    => String
    -> Sem r FilePath
resolveTemplateNameRelative tName = do
    tDir <- PR.asks @SiteGenConfig sgcTemplatesDir
    makeRelative tDir <$> resolveTemplateName tName


-- | resolve the actual path using the tPath and tDir.  If tDir is a
-- subdirectory of tPAth, remove it, and then check it directly, and then with
-- the _defaults in front of it. If we can't resolve it return Nothing
resolveTemplatePath
    :: Members '[ File
                , Log String
                ] r
    => FilePath
    -> FilePath
    -> Sem r (Maybe FilePath)
resolveTemplatePath tDir tPath = do
    CP.log $ "resolveTemplatePath for: " <> tPath <> " at " <> tDir
    let relPath = if isRelative tPath then tPath else makeRelative tDir tPath
        fileName = takeFileName relPath
        hasPath = pathSeparator `elem` relPath
        tryPaths = map (normalise . (tDir </>))
            $ if hasPath
                -- try actual, then with _defaults, and then _defaults/fileName
                then [relPath, "_defaults" </> relPath, "_defaults" </> fileName]
                else [relPath, "_defaults" </> relPath]
    {-CP.log "Trying paths:"-}
    {-CP.log $ intercalate "\n" tryPaths-}
    exists <- forM tryPaths EF.doesFileExist
    let pairs = dropWhile (not.snd) $ zip tryPaths exists
    if null pairs
      then do
          CP.log "Couldn't find a file"
          pure Nothing
      else do
          let rFileName = fst $ head pairs
          CP.log $ "Using " <> show rFileName
          pure $ Just rFileName



-- now some test code to see if it works


testResolveTemplatePathForSC
    :: Members '[ File
                , Error SiteGenError
                , Error FileException
                , Error ConfigException
                , Log String
                ] r
    => Sem r FilePath
testResolveTemplatePathForSC = do
    sgc <- getSiteGenConfig "./example-site/site.yaml" False
    let file = "./example-site/src/posts/test_post.md"
    fp <- EF.makeAbsolute file
    PR.runReader @SiteGenConfig sgc $ do
        mSpc <- filePathToMaybeSourcePageContext fp
        case mSpc of
            Nothing -> throw $ EF.FileException file "Not a sitegen file"
            (Just spc) -> do
                let sgr = makeSiteGenReader [H.SPC spc]
                PR.runReader @SiteGenReader sgr
                    $ PR.runReader @SiteGenConfig sgc
                    $ resolveTemplateNameForSC (H.SPC spc)


-- Run the Sem r monad to IO
runTest x = x & EF.fileToIO
              & PE.mapError @FileException mapSiteGenError
              & PE.mapError @ConfigException mapSiteGenError
              & PE.errorToIOFinal @SiteGenError
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal @IO


testRP = testResolveTemplatePathForSC & runTest
