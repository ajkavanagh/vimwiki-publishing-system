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
    ( renderSourceMetadata
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

import           System.FilePath        (normalise, (</>))

import           Control.Monad          (when)

import           Data.Maybe             (fromJust, isJust, isNothing)
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP
import           Polysemy               (Member, Members, Sem)
import           Polysemy.Error         (Error)
import qualified Polysemy.Error         as PE
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)
import qualified Polysemy.State         as PS

import           Text.Ginger            (SourcePos, Template)
import           Text.Pandoc            (Pandoc)

import           Effect.Cache           (Cache)
import           Effect.File            (File, FileException)
import           Effect.Locale          (Locale)
import           Effect.Logging         (LoggingMessage)
import qualified Effect.Logging         as EL
import           Effect.Print           (Print)

import           Types.Errors           (SiteGenError (..))
import           Types.Ginger           (GingerException (..))
import           Types.Header           (SourceMetadata (..))

import           Lib.Context            (makeContextFor)
import           Lib.Files              (ensureDirectoriesExistFor,
                                         writeAndMemo)
import           Lib.Ginger             (parseToTemplate, renderTemplate)
import           Lib.ResolvingTemplates (resolveTemplateNameForSM,
                                         resolveTemplateNameRelative)
import           Lib.RouteUtils         (makeFileNameFrom)
import           Lib.SiteGenConfig      (ConfigException, SiteGenConfig (..))
import           Lib.SiteGenState       (SiteGenReader (..), SiteGenState (..),
                                         addToSitePagesRendered)


renderSourceMetadata
    :: ( Member File r
       , Member Locale r
       , Member (Cache Pandoc) r
       , Member (Cache (Template SourcePos)) r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error GingerException) r
       , Member (Error FileException) r
       , Member (Error ConfigException) r
       , Member (Log LoggingMessage) r
       , Member Print r
       )
    => SourceMetadata
    -> Sem r ()
renderSourceMetadata sm = do
    let fp = smRelFilePath sm
    EL.logInfo $ T.pack $ "Rendering route: "
                       ++ smRoute sm
                       ++ (if isJust fp then " for file: " ++ (fromJust fp)
                                        else " - no source file.")
    --
    -- find the template
    template <- resolveTemplateNameRelative (smTemplate sm)
    tplt <- parseToTemplate template

    -- build a Ginger context  -- this contains functions for content()
    -- summary(), toc(), etc.
    ctxt <- makeContextFor sm

    -- render the file (this is one that accepts the context the template and
    -- returns the output as a string or throws an error.  Note calling
    -- renderTemplate will cause the Pandoc document to be read if the content,
    -- toc or summary is needed for something.
    out <- renderTemplate ctxt tplt

    -- and arrange to write the file and record what we have written.  This
    -- might include creating directories, etc. and making a note that we've
    -- written the file.
    writeOutputFile sm out

    -- Finally make a note that we've actually rendered that route.
    addToSitePagesRendered sm


writeOutputFile
    :: ( Member File r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error FileException) r
       , Member (Log LoggingMessage) r
       )
    => SourceMetadata
    -> Text
    -> Sem r ()
writeOutputFile sm txt = do
    sgc <- PR.ask @SiteGenConfig
    -- need to construct the relative filename
    let doIndexFiles = sgcIndexFiles sgc
        ext = sgcOutputFileExt sgc
        dir = sgcOutputDir sgc
        relFileName = makeFileNameFrom doIndexFiles ext sm
        absFileName = normalise (dir </> relFileName)
    EL.logInfo $ T.pack $ " Writing to --> " ++ relFileName
    --EL.logDebug $ T.pack $ " --> " ++ absFileName
    -- Need to check that the base output directory exists
    -- Need to test and create any intermediate directories
    ensureDirectoriesExistFor dir relFileName
    -- Then write the file.
    writeAndMemo dir relFileName txt
