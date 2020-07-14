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

import           Types.Context     (Context, RunSem, RunSemGVal)
import           Types.Errors      (SiteGenError)
import           Types.Header      (SourceMetadata (..))

import           Lib.Context.Core  (contextFromList, tryExtractIntArg)
import           Lib.Pandoc        (smContentM, smSummaryM, smTocM)
import           Lib.SiteGenConfig (SiteGenConfig)
import           Lib.SiteGenState  (SiteGenReader, SiteGenState)


-- Basically, this module provides the 'content', 'summary' and 'toc' html
-- fragments for a SourceMetadata.


pageFunctionsContext
    :: GingerSemEffects r
    => SourceMetadata
    -> Context (RunSem r)
pageFunctionsContext sm = contextFromList
    [ ("content",  funcDynamicMGValM contentDynamic sm)
    , ("summary", funcDynamicMGValM summaryDynamic sm)
    , ("toc", funcDynamicMGValM tocDynamic sm)]


pageFunctionsAsGValDict
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.GVal (RunSem r)
pageFunctionsAsGValDict sm =
    TG.dict [ ("content", TG.fromFunction (contentDynamic sm))
            , ("summary", TG.fromFunction (summaryDynamic sm))
            , ("toc",     TG.fromFunction (tocDynamic sm))
            ]


-- helper function to hook a function dynamically for the required thing.
funcDynamicMGValM
    :: GingerSemEffects r
    => (SourceMetadata -> TG.Function (RunSem r))
    -> SourceMetadata
    -> RunSemGVal r
funcDynamicMGValM f sm = pure $ TG.fromFunction $ f sm


-- | fetch the content dynamically as a function call from the context.  i.e. it
-- has to be called as "content()". Note it provides RAW html, not text.
contentDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
contentDynamic sm _ = do           -- content ignores the args
    --TG.liftRun $ EL.logDebug "Dynamic content was asked for"
    txt <- TG.liftRun $ smContentM sm
    pure $ TG.toGVal $TGH.unsafeRawHtml txt


-- | fetch the summary of the page, as raw html
-- TODO: match the args for plain=True to select for plain summary rather than
--       rich summary
summaryDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
summaryDynamic sm _ = do   -- summary ignores the args, and selects for Rich only
    txt <- TG.liftRun $ smSummaryM sm True
    pure $ TG.toGVal $ TGH.unsafeRawHtml txt


-- | fetch the table of contents for the page.
-- The argument is the level as an Integer which determines how many levels to
-- return in the RAW HTML fragment.
tocDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
tocDynamic sm args = do
    let mLevels = tryExtractIntArg args
    txt <- TG.liftRun $ smTocM sm mLevels
    pure $ TG.toGVal $ TGH.unsafeRawHtml txt
