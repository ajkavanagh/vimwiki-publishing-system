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


module Lib.Context.DynamicContexts where

import           Data.Maybe        (isNothing)
import           Data.Text         (Text)

import           Text.Ginger       ((~>))
import qualified Text.Ginger       as TG
import qualified Text.Ginger.Html  as TGH

import           Colog.Polysemy    (Log)
import qualified Colog.Polysemy    as CP
import           Polysemy          (Member, Sem)
import           Polysemy.Error    (Error)
import           Polysemy.Reader   (Reader)
import           Polysemy.State    (State)
import           Polysemy.Writer   (Writer)

import           Effect.File       (File)
import           Effect.Ginger     (GingerSemEffects)
import qualified Effect.Logging    as EL

import           Lib.Context.Core  (contextFromList, tryExtractIntArg)
import           Lib.Errors        (SiteGenError)
import qualified Lib.Header        as H
import           Lib.Pandoc        (scContentM, scSummaryM, scTocM)
import           Lib.SiteGenConfig (SiteGenConfig)
import           Lib.SiteGenState  (SiteGenReader, SiteGenState)
import           Types.Context     (Context, RunSem, RunSemGVal)


-- Basically, this module provides the 'content', 'summary' and 'toc' html
-- fragments for a SourceContext (although, it's a bit boring for the
-- VirtualPageContext.
--
-- These are determined on demand via the SourceClass (which is provided)


pageFunctionsContext
    :: GingerSemEffects r
    => H.SourceContext
    -> Context (RunSem r)
pageFunctionsContext sc = contextFromList
    [ ("content",  funcDynamicMGValM contentDynamic sc)
    , ("summary", funcDynamicMGValM summaryDynamic sc)
    , ("toc", funcDynamicMGValM tocDynamic sc)]


pageFunctionsAsGValDict
    :: GingerSemEffects r
    => H.SourceContext
    -> TG.GVal (RunSem r)
pageFunctionsAsGValDict sc =
    TG.dict [ ("content", TG.fromFunction (contentDynamic sc))
            , ("summary", TG.fromFunction (summaryDynamic sc))
            , ("toc",     TG.fromFunction (tocDynamic sc))
            ]


-- helper function to hook a function dynamically for the required thing.
funcDynamicMGValM
    :: GingerSemEffects r
    => (H.SourceContext -> TG.Function (RunSem r))
    -> H.SourceContext
    -> RunSemGVal r
funcDynamicMGValM f sc = pure $ TG.fromFunction $ f sc


-- | fetch the content dynamically as a function call from the context.  i.e. it
-- has to be called as "content()". Note it provides RAW html, not text.
contentDynamic
    :: GingerSemEffects r
    => H.SourceContext
    -> TG.Function (RunSem r)
contentDynamic sc _ = do           -- content ignores the args
    --TG.liftRun $ EL.logDebug "Dynamic content was asked for"
    txt <- TG.liftRun $ scContentM sc
    pure $ TG.toGVal $TGH.unsafeRawHtml txt


-- | fetch the summary of the page, as raw html
-- TODO: match the args for plain=True to select for plain summary rather than
--       rich summary
summaryDynamic
    :: GingerSemEffects r
    => H.SourceContext
    -> TG.Function (RunSem r)
summaryDynamic sc _ = do   -- summary ignores the args, and selects for Rich only
    txt <- TG.liftRun $ scSummaryM sc True
    pure $ TG.toGVal $ TGH.unsafeRawHtml txt


-- | fetch the table of contents for the page.
-- The argument is the level as an Integer which determines how many levels to
-- return in the RAW HTML fragment.
tocDynamic
    :: GingerSemEffects r
    => H.SourceContext
    -> TG.Function (RunSem r)
tocDynamic sc args = do
    let mLevels = tryExtractIntArg args
    txt <- TG.liftRun $ scTocM sc mLevels
    pure $ TG.toGVal $ TGH.unsafeRawHtml txt
