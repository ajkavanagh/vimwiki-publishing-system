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


module Lib.RenderUtils
    ( renderSourceContext
    )
    where

-- The complexity here, is what is this for?
--
-- We're trying to model:
--
--  * Pages from .md files
--  * Virtual pages like the 404.html that might not have a 404.md
--  * Virtual index pages for routes that don't have an index page.
--  * Virtual index pages for categories and tags.
--  * Auto pages like diaries
--  * Something to do with fragment pages.
--
-- And then we want to tie this to the template and then the Ginger context to
-- actually be able to render it.  The Ginger context we build on the fly.  Is
-- that what this is?
--

import           TextShow

import           System.FilePath        (joinPath, normalise, splitDirectories,
                                         takeDirectory, (</>))

import           Control.Monad          (forM_, unless)

import qualified Data.HashSet           as HashSet
import qualified Data.List              as L
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Encoding     (encodeUtf8)

import           Colog.Core             (logStringStderr)
import           Colog.Polysemy         (Log, runLogAction)
import qualified Colog.Polysemy         as CP
import           Polysemy               (Embed, Member, Members, Sem, embed,
                                         embedToFinal, interpret, makeSem, run,
                                         runFinal)
import           Polysemy.Error         (Error)
import qualified Polysemy.Error         as PE
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)
import qualified Polysemy.State         as PS
import           Polysemy.Writer        (Writer)
import qualified Polysemy.Writer        as PW

import           Effect.ByteStringStore (ByteStringStore)
import qualified Effect.ByteStringStore as EB
import           Effect.File            (File, FileException)
import qualified Effect.File            as EF
import           Effect.Ginger          (GingerException (..))

import           Lib.Context            (makeContextFor)
import           Lib.Errors             (SiteGenError)
import           Lib.Ginger             (parseToTemplate, renderTemplate)
import           Lib.Header             (SourceContext)
import qualified Lib.Header             as H
import           Lib.ResolvingTemplates as RT
import           Lib.RouteUtils         (makeFileNameFrom)
import           Lib.SiteGenConfig      (SiteGenConfig (..))
import           Lib.SiteGenState       (FileMemo (..), SiteGenReader (..),
                                         SiteGenState (..), recordMemo)


renderSourceContext
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error GingerException) r
       , Member (Error FileException) r
       , Member (Log String) r
       )
    => SourceContext
    -> Sem r ()
renderSourceContext sc = do
    CP.log @String $ "renderSourceContext for route: "
                  ++ H.scRoute sc
                  ++ ", file: "
                  ++ show (H.scRelFilePath sc)
    -- find the template
    tplt <- parseToTemplate =<< RT.resolveTemplatePath sc

    -- build a Ginger context  -- this contains functions for content()
    -- summary(), toc(), etc.
    ctxt <- makeContextFor sc

    -- render the file (this is one that accepts the context the template and
    -- returns the output as a string or throws an error.  Note calling
    -- renderTemplate will cause the Pandoc document to be read if the content,
    -- toc or summary is needed for something.
    out <- renderTemplate ctxt tplt

    -- and arrange to write the file and record what we have written.  This
    -- might include creating directories, etc. and making a note that we've
    -- written the file.
    writeOutputFile sc out



writeOutputFile
    :: ( Member File r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error FileException) r
       , Member (Log String) r
       )
    => SourceContext
    -> Text
    -> Sem r ()
writeOutputFile sc txt = do
    sgc <- PR.ask @SiteGenConfig
    -- need to construct the relative filename
    let doIndexFiles = sgcIndexFiles sgc
        ext = sgcOutputFileExt sgc
        dir = sgcOutputDir sgc
        relFileName = makeFileNameFrom doIndexFiles ext sc
        absFileName = normalise (dir </> relFileName)
    CP.log @String $ " --> " <> relFileName
    CP.log @String $ " --> " <> absFileName
    CP.log @String "The output was"
    CP.log @String (T.unpack txt)
    -- Need to check that the base output directory exists
    -- Need to test and create any intermediate directories
    ensureDirectoriesExistFor dir relFileName
    -- Then write the file.
    writeAndMemo dir relFileName txt


-- | ensure that the directories exist for the file we are about to write.  If
-- they don't then create them.  If we can't then try to create them; any errors
-- will cause a File Exception.  The function also makes a note of all the
-- intermediate directories in the memoFiles state (in SiteGenState)
ensureDirectoriesExistFor
    :: ( Member File r
       , Member (State SiteGenState) r
       , Member (Log String) r
       , Member (Error FileException) r
       )
    => FilePath     -- The base directory; we've already checked, but doing again does no harm
    -> FilePath     -- the whole additional path including the file
    -> Sem r ()     -- this function is run for it's side-effect.
ensureDirectoriesExistFor base relFile = do
    let dir = takeDirectory (normalise relFile)
        paths = tail (L.inits (splitDirectories dir))
    -- ensure the directories all exist
    forM_ paths $ \pathList -> do
        let path = normalise (joinPath pathList)
            dir' = normalise (base </> path)
        exists <- EF.doesDirectoryExist dir'
        unless exists $ do
            CP.log $ "Directory " <> dir' <> " doesn't exist, creating"
            EF.createDirectory dir'
        -- as the directory create worked (no exception), or alreadly exists,
        -- create the memo for it.
        recordMemo $ DirMemo path


-- | write the file and memo it, so we know what was written.
writeAndMemo
    :: ( Member File r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error FileException) r
       , Member (Log String) r
       )
    => FilePath
    -> FilePath
    -> Text
    -> Sem r ()
writeAndMemo dir relFile txt = do
    let fullpath = normalise (dir </> relFile)
        nFile = normalise relFile
    CP.log $ "Writing output to " <> nFile
    {-CP.log $ "Writing output to " <> fullpath-}
    EF.writeFile fullpath (encodeUtf8 txt)
    -- if the above passed, the memo the file
    recordMemo $ FileMemo nFile
