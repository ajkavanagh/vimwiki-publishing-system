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


module Experiments.ResolvingTemplates where

import           System.FilePath  (FilePath, pathSeparator, takeDirectory, splitPath, joinPath,
                                   (</>), (<.>))

import           Control.Monad    (forM)
import           Data.List        (inits, dropWhile)

import           Polysemy         (Embed, Members, Sem, embed, embedToFinal, runFinal)
import           Polysemy.Error   (Error, throw)
import           Polysemy.Reader  (Reader)
import qualified Polysemy.Reader  as PR

import           Effect.File      (File, FileException)
import qualified Effect.File      as EF

import           Lib.Header       (SourcePageHeader(..))
import           Lib.SiteGenConfig (SiteGenConfig(..), getSiteGenConfig, ConfigException)
import           Lib.Errors       (SiteGenError(..))

-- this is going to move
import           SiteGenState     (SiteGenReader, siteGenConfig)

-- for tests -- remove when removing test code
import           Lib.Files        (filePathToMaybeSourcePageHeader)
import           Colog.Polysemy       (Log, runLogAction)
import qualified Colog.Polysemy       as CP
import           Colog.Core           (logStringStderr)
import           SiteGenState     (makeSiteGenReader)
import qualified Polysemy.Error       as PE
import           Lib.Errors        (mapSiteGenError)
import           Data.Function        ((&))
import           Data.List (intercalate)

{-
   So the idea here is that we have a templates directory, and in the templates
   directory we will have (possibly) subdirectories and template files.

    ./templates/
        default.html.j2
        index.html.j2
        posts/
            index.html.j2

   So the idea is that in 'posts' and file that is asking for the 'default'
   template will end up with the './templates/default.html.j2' templates, but if
   asked for the 'index' template, it would get './templates/posts/index.html.j2'

-}

-- | attempt to resolve the template.  If no template is found to find the
-- default template, as defined in the SiteGenConfig
resolveTemplatePath
    :: Members '[ Reader SiteGenReader
                , File
                , Error SiteGenError
                ] r
    => SourcePageHeader
    -> Sem r FilePath
resolveTemplatePath sph = do
    sgc <- PR.asks @SiteGenReader siteGenConfig
    let tBaseName = phTemplate sph
        ext = sgcTemplateExt sgc
        tFileName = tBaseName <.> sgcTemplateExt sgc
        sPath = phFileName sph
        dir = takeDirectory sPath
        dirParts = splitPath dir
        tPath = sgcTemplatesDir sgc
        -- now got tPath for the templates and the parts of the path.
        dirPaths = reverse $ inits dirParts
        tryPaths = map (\ps -> tPath </> joinPath ps </> tFileName) dirPaths
    exists <- forM tryPaths EF.doesFileExist
    let pairs = dropWhile (not.snd) $ zip tryPaths exists
    if null pairs
      then throw $ PageError sph "Couldn't find a template"
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
    PR.runReader @SiteGenConfig sgc $ do
        mSph <- filePathToMaybeSourcePageHeader file
        case mSph of
            Nothing -> throw $ EF.FileException file "Not a sitegen file"
            (Just sph) -> do
                let sgr = makeSiteGenReader sgc [sph]
                PR.runReader @SiteGenReader sgr $ resolveTemplatePath sph


-- Run the Sem r monad to IO
runTest x = x & EF.fileToIO
              & PE.mapError @FileException mapSiteGenError
              & PE.mapError @ConfigException mapSiteGenError
              & PE.errorToIOFinal @SiteGenError
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal @IO


testRP = testResolveTemplatePath & runTest
