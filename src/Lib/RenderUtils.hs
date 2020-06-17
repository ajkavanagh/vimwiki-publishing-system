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

import           Data.Text              (Text)
import qualified Data.Text              as T

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

import           Lib.SiteGenConfig      (SiteGenConfig, sgcIndexFiles)

import           Lib.Errors             (SiteGenError)
import           Lib.Header             (SourceContext)
import qualified Lib.Header             as H
import           Lib.ResolvingTemplates as RT
import           Lib.SiteGenState       (SiteGenReader, SiteGenState)
import           Lib.RouteUtils         (makeFileNameFrom)

import           Lib.Context            (makeContextFor)
import           Lib.Ginger             (parseToTemplate, renderTemplate)


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
    doIndexFiles <- PR.asks @SiteGenConfig sgcIndexFiles
    CP.log @String $ " --> " <> makeFileNameFrom doIndexFiles ".html" sc
    CP.log @String "The output was"
    CP.log @String (T.unpack txt)
    -- need to construct the relative filename
    -- Need to check that the base output directory exists
    -- Need to test and create any intermediate directories
    -- Then write the file.


{- 
    Work out where files go.

    Firstly: No index-files

    Route:                File Name
    ------                ---------
    /                     index.html
    hello                 hello.html
    hello/                hello/index.html
    hello/there           hello/there.html
 
    Secondly: With index-files
   
    Route:                File Name
    ------                ---------
    /                     index.html
    hello                 hello/index.html
    hello/                hello/index.html  -- clash!
    hello/there           hello/there/index.html

    So duplicate routes have to check for two routes where "thing" and "thing/"
    both exist as otherwise there will be an error!
-}