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
                                    pathSeparator, splitPath, takeDirectory,
                                    (<.>), (</>))

import           Control.Monad     (forM)

import           Polysemy          (Embed, Members, Sem, embed, embedToFinal,
                                    runFinal)
import           Polysemy.Error    (Error, throw)
import           Polysemy.Reader   (Reader)
import qualified Polysemy.Reader   as PR

import           Effect.File       (File, FileException)
import qualified Effect.File       as EF

import           Lib.Header        (SourcePageContext (..))
import           Lib.SiteGenConfig (ConfigException, SiteGenConfig (..),
                                    getSiteGenConfig)
-- for tests -- remove when removing test code
import           Colog.Core        (logStringStderr)
import           Colog.Polysemy    (Log, runLogAction)
import qualified Colog.Polysemy    as CP

import           Data.Function     ((&))
import           Data.List         (dropWhile, inits, intercalate)

import           Lib.Errors        (SiteGenError (..), mapSiteGenError)
import           Lib.Files         (filePathToMaybeSourcePageContext)
import           Lib.SiteGenState  (SiteGenReader, makeSiteGenReader,
                                    siteGenConfig)
import qualified Lib.SourceClass   as SC

import qualified Polysemy.Error    as PE

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

-- | attempt to resolve the template.  If no template is found to find the
-- default template, as defined in the SiteGenConfig
resolveTemplatePath
    :: Members '[ Reader SiteGenReader
                , File
                , Error SiteGenError
                , Log String
                ] r
    => SourcePageContext
    -> Sem r FilePath
resolveTemplatePath spc = do
    sgc <- PR.asks @SiteGenReader siteGenConfig
    let tBaseName = spcTemplate spc
        hasPath = pathSeparator `elem` tBaseName
        tFileName = tBaseName <.> sgcTemplateExt sgc
        sPath = spcRelFilePath spc
        dir = takeDirectory sPath
        tPath = sgcTemplatesDir sgc
        tryPaths = map (tPath </>) $ case (isRelative tFileName, hasPath) of
            (True, True) -> [ tFileName, "_defaults" </> tFileName ]
            (True, False) -> [ dir </> tFileName
                             , "_defaults" </> dir </> tFileName
                             , "_defaults" </> tFileName ]
            (False, _) -> [ tFileName]
    CP.log $ intercalate "\n" tryPaths
    exists <- forM tryPaths EF.doesFileExist
    let pairs = dropWhile (not.snd) $ zip tryPaths exists
    if null pairs
      then throw $ SourcePageContextError spc "Couldn't resolve a template"
      else pure $ fst $ head pairs


-- now some test code to see if it works


testResolveTemplatePath
    :: Members '[ File
                , Error SiteGenError
                , Error FileException
                , Error ConfigException
                , Log String
                ] r
    => Sem r FilePath
testResolveTemplatePath = do
    sgc <- getSiteGenConfig "./example-site/site.yaml" False
    let file = "./example-site/src/posts/test_post.md"
    fp <- EF.makeAbsolute file
    PR.runReader @SiteGenConfig sgc $ do
        mSpc <- filePathToMaybeSourcePageContext fp
        case mSpc of
            Nothing -> throw $ EF.FileException file "Not a sitegen file"
            (Just spc) -> do
                let sgr = makeSiteGenReader sgc [SC.SPC spc]
                PR.runReader @SiteGenReader sgr $ resolveTemplatePath spc


-- Run the Sem r monad to IO
runTest x = x & EF.fileToIO
              & PE.mapError @FileException mapSiteGenError
              & PE.mapError @ConfigException mapSiteGenError
              & PE.errorToIOFinal @SiteGenError
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal @IO


testRP = testResolveTemplatePath & runTest
