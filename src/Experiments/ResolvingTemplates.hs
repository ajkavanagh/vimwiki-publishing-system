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

import           System.FilePath   (FilePath, joinPath, pathSeparator,
                                    splitPath, takeDirectory, (<.>), (</>))

import           Control.Monad     (forM)
import           Data.List         (dropWhile, inits)

import           Polysemy          (Embed, Members, Sem, embed, embedToFinal,
                                    runFinal)
import           Polysemy.Error    (Error, throw)
import           Polysemy.Reader   (Reader)
import qualified Polysemy.Reader   as PR

import           Effect.File       (File, FileException)
import qualified Effect.File       as EF

import           Lib.Errors        (SiteGenError (..))
import           Lib.Header        (SourcePageContext (..))
import           Lib.SiteGenConfig (ConfigException, SiteGenConfig (..),
                                    getSiteGenConfig)

-- this is going to move
import           Lib.SiteGenState  (SiteGenReader, siteGenConfig)

-- for tests -- remove when removing test code
import           Colog.Core        (logStringStderr)
import           Colog.Polysemy    (Log, runLogAction)
import qualified Colog.Polysemy    as CP
import           Data.Function     ((&))
import           Data.List         (intercalate)
import           Lib.Errors        (mapSiteGenError)
import           Lib.Files         (filePathToMaybeSourcePageContext)
import           Lib.SiteGenState  (makeSiteGenReader)
import qualified Polysemy.Error    as PE

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
                , Log String
                ] r
    => SourcePageContext
    -> Sem r FilePath
resolveTemplatePath spc = do
    sgc <- PR.asks @SiteGenReader siteGenConfig
    let tBaseName = spcTemplate spc
        ext = sgcTemplateExt sgc
        tFileName = tBaseName <.> sgcTemplateExt sgc
        sPath = spcRelFilePath spc
        dir = takeDirectory sPath
        dirParts = splitPath dir
        tPath = sgcTemplatesDir sgc
        -- now got tPath for the templates and the parts of the path.
        dirPaths = reverse $ inits dirParts
        tryPaths = map (\ps -> tPath </> joinPath ps </> tFileName) dirPaths
    CP.log $ intercalate "\n" tryPaths
    exists <- forM tryPaths EF.doesFileExist
    let pairs = dropWhile (not.snd) $ zip tryPaths exists
    if null pairs
      then throw $ PageError spc "Couldn't find a template"
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
                let sgr = makeSiteGenReader sgc [spc]
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
