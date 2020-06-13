{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.PageContexts where

import Data.Time.Clock  (UTCTime)
import Data.Time.LocalTime (LocalTime, utc, utcToLocalTime)

import           Text.Ginger      ((~>))
import qualified Text.Ginger      as TG

import qualified Lib.Header       as H
import qualified Lib.SourceClass  as SC
import           Lib.Context.Core (Context, contextFromList)

-- Provide contexts for the SourcePageContext and the VirtualPageContext records
-- They will be provided under the key 'header'

pageHeaderContextFor :: Monad m => SC.SourceContext -> Context m
pageHeaderContextFor (SC.SPC spc) = sourcePageContextFor spc
pageHeaderContextFor (SC.VPC vpc) = virtualPageContextFor vpc


sourcePageContextFor :: Monad m => H.SourcePageContext -> Context m
sourcePageContextFor spc = contextFromList [("header", sourcePageContextM spc)]


sourcePageContextM :: Monad m => H.SourcePageContext -> m (TG.GVal m)
sourcePageContextM spc =
    pure $ TG.dict
        [ "route"           ~> H.spcRoute spc
        , "absFilePath"     ~> H.spcAbsFilePath spc
        , "relFilePath"     ~> H.spcRelFilePath spc
        , "vimWikiLinkPath" ~> H.spcVimWikiLinkPath spc
        , "title"           ~> H.spcTitle spc
        , "template"        ~> H.spcTemplate spc
        , "tags"            ~> H.spcTags spc
        , "category"        ~> H.spcCategory spc
        , "date"            ~> (tempToLocalTimeHelper <$> H.spcDate spc)
        , "updated"         ~> (tempToLocalTimeHelper <$> H.spcUpdated spc)
        , "indexPage"       ~> H.spcIndexPage spc
        , "authors"         ~> H.spcAuthors spc
        , "publish"         ~> H.spcPublish spc
        , "siteId"          ~> H.spcSiteId spc
        ]



virtualPageContextFor :: Monad m => H.VirtualPageContext -> Context m
virtualPageContextFor vpc = contextFromList [("header", virtualPageContextM vpc)]


virtualPageContextM :: Monad m => H.VirtualPageContext -> m (TG.GVal m)
virtualPageContextM vpc =
    pure $ TG.dict
        [ "route"           ~> H.vpcRoute vpc
        , "vimWikiLinkPath" ~> H.vpcVimWikiLinkPath vpc
        , "title"           ~> H.vpcTitle vpc
        , "template"        ~> H.vpcTemplate vpc
        , "date"            ~> (tempToLocalTimeHelper <$> H.vpcDate vpc)
        , "updated"         ~> (tempToLocalTimeHelper <$> H.vpcUpdated vpc)
        , "indexPage"       ~> H.vpcIndexPage vpc
        , "publish"         ~> H.vpcPublish vpc
        ]


-- TODO: helper until I work out what to do with UTC time in the app, and how to
-- present it in pages.
tempToLocalTimeHelper :: UTCTime -> LocalTime
tempToLocalTimeHelper = utcToLocalTime utc
