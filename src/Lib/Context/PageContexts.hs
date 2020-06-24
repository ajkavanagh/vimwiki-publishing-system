{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.PageContexts where

import           Data.Time.Clock     (UTCTime)
import           Data.Time.LocalTime (LocalTime, utc, utcToLocalTime)

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP
import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)
import           Polysemy.Writer        (Writer)

import           Effect.ByteStringStore (ByteStringStore)
import           Effect.File            (File)

import           Text.Ginger         ((~>))
import qualified Text.Ginger         as TG

import           Lib.Context.Core       (Context, RunSem, RunSemGVal,
                                         contextFromList, tryExtractIntArg)
import qualified Lib.Header          as H
import           Lib.Errors             (SiteGenError)
import           Lib.SiteGenConfig      (SiteGenConfig)
import           Lib.SiteGenState       (SiteGenReader (..), SiteGenState)
import           Lib.Context.DynamicContexts (contentDynamic, tocDynamic, summaryDynamic)

-- Provide contexts for the SourcePageContext and the VirtualPageContext records
-- They will be provided under the key 'header'

pageHeaderContextFor -- :: Monad m => H.SourceContext -> Context m
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourceContext
    -> Context (RunSem r)
pageHeaderContextFor (H.SPC spc) = sourcePageContextFor spc
pageHeaderContextFor (H.VPC vpc) = virtualPageContextFor vpc


sourcePageContextFor
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> Context (RunSem r)
sourcePageContextFor spc =
    contextFromList
        $  [("Page", sourcePageContextM spc)]
        {-++ [("Pages", pagesContextM (H.SPC spc)) | H.spcIndexPage spc]-}


sourcePageContextM
    -- :: Monad m
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> RunSemGVal r
sourcePageContextM spc = do
    TG.liftRun $ CP.log @String $ "Building sourcePageContextM for " ++ show (H.spcRoute spc)
    pure $ TG.dict
        [ "Route"           ~> H.spcRoute spc
        , "AbsFilePath"     ~> H.spcAbsFilePath spc
        , "RelFilePath"     ~> H.spcRelFilePath spc
        , "VimWikiLinkPath" ~> H.spcVimWikiLinkPath spc
        , "Title"           ~> H.spcTitle spc
        , "Template"        ~> H.spcTemplate spc
        , "Tags"            ~> H.spcTags spc
        , "Category"        ~> H.spcCategory spc
        , "Date"            ~> (tempToLocalTimeHelper <$> H.spcDate spc)
        , "Updated"         ~> (tempToLocalTimeHelper <$> H.spcUpdated spc)
        , "IndexPage"       ~> H.spcIndexPage spc
        , "Authors"         ~> H.spcAuthors spc
        , "Publish"         ~> H.spcPublish spc
        , "Draft"           ~> not (H.spcPublish spc)
        , "SiteId"          ~> H.spcSiteId spc
        -- note these are lower case initial as they are functions and need to
        -- be called from the template
        , ("content",          TG.fromFunction (contentDynamic (H.SPC spc)))
        , ("summary",          TG.fromFunction (summaryDynamic (H.SPC spc)))
        , ("toc",              TG.fromFunction (tocDynamic (H.SPC spc)))
        ]


-- | Construct a list of Pages for all of the direct sub-pages in this
-- collection.  The sub-pages are the pages in the same route that share the
-- same index.  We may also include the any index pages.  Sort by index pages
-- and then by alpha.  So if we have / and /thing and /thing/after then /thing
-- will be in Pages, but /thing/after won't be.  Obviously, every route has to
-- start with /, but then they all should.
-- TODO: finish this after we've worked out pagination.
pagesContextM
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourceContext
    -> RunSemGVal r
pagesContextM sc = do
    scs <- TG.liftRun $ PR.asks @SiteGenReader siteSourceContexts
    let route = H.scRoute sc
    undefined



virtualPageContextFor :: Monad m => H.VirtualPageContext -> Context m
virtualPageContextFor vpc = contextFromList [("Page", virtualPageContextM vpc)]


virtualPageContextM :: Monad m => H.VirtualPageContext -> m (TG.GVal m)
virtualPageContextM vpc =
    pure $ TG.dict
        [ "Route"           ~> H.vpcRoute vpc
        , "VimWikiLinkPath" ~> H.vpcVimWikiLinkPath vpc
        , "Title"           ~> H.vpcTitle vpc
        , "Template"        ~> H.vpcTemplate vpc
        , "Date"            ~> (tempToLocalTimeHelper <$> H.vpcDate vpc)
        , "Updated"         ~> (tempToLocalTimeHelper <$> H.vpcUpdated vpc)
        , "IndexPage"       ~> H.vpcIndexPage vpc
        , "Publish"         ~> H.vpcPublish vpc
        , "Draft"           ~> not (H.vpcPublish vpc)
        ]


-- TODO: helper until I work out what to do with UTC time in the app, and how to
-- present it in pages.
tempToLocalTimeHelper :: UTCTime -> LocalTime
tempToLocalTimeHelper = utcToLocalTime utc
