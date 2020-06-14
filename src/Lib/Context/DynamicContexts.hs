{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.DynamicContexts where

import           Data.Maybe             (isNothing)
import           Data.Scientific        (Scientific, toBoundedInteger)
import           Data.Text              (Text)

import           Text.Ginger            ((~>))
import qualified Text.Ginger            as TG

import           Polysemy               (Member, Sem)
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR

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
import           Effect.File            (File)
import qualified Effect.File            as EF

import           Lib.Pandoc             (scContentM, scSummaryM, scTocM)
import           Lib.Context.Core       (Context, RunSem, RunSemGVal,
                                         contextFromList)
import           Lib.Errors             (SiteGenError)
import           Lib.SiteGenConfig      (SiteGenConfig)
import qualified Lib.SiteGenConfig      as S
import           Lib.SiteGenState       (SiteGenReader, SiteGenState)
import qualified Lib.SourceClass        as SC


-- Basically, this module provides the 'content', 'summary' and 'toc' html
-- fragments for a SourceContext (although, it's a bit boring for the
-- VirtualPageContext.
--
-- These are determined on demand via the SourceClass (which is provided)


pageFunctionsContext
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SC.SourceContext
    -> Context (RunSem r)
pageFunctionsContext sc = contextFromList
    [ ("content",  funcDynamicMGValM contentDynamic sc)
    , ("summary", funcDynamicMGValM summaryDynamic sc)
    , ("toc", funcDynamicMGValM tocDynamic sc)]


pageFunctionsAsGValDict
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SC.SourceContext
    -> TG.GVal (RunSem r)
pageFunctionsAsGValDict sc =
    TG.dict [ ("content", TG.fromFunction (contentDynamic sc))
            , ("summary", TG.fromFunction (summaryDynamic sc))
            , ("toc",     TG.fromFunction (tocDynamic sc))
            ]


-- helper function to hook a function dynamically for the required thing.
funcDynamicMGValM
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => (SC.SourceContext -> TG.Function (RunSem r))
    -> SC.SourceContext
    -> RunSemGVal r
funcDynamicMGValM f sc = pure $ TG.fromFunction $ f sc


-- | fetch the content dynamically as a function call from the context.  i.e. it
-- has to be called as "content()", probably with a "| raw" filter.
contentDynamic
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SC.SourceContext
    -> TG.Function (RunSem r)
contentDynamic sc _ = do           -- content ignores the args
    txt <- TG.liftRun $ scContentM sc
    pure $ TG.toGVal txt


-- | fetch the summary of the page.
-- TODO: match the args for plain=True to select for plain summary rather than
--       rich summary
summaryDynamic
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SC.SourceContext
    -> TG.Function (RunSem r)
summaryDynamic sc _ = do   -- summary ignores the args, and selects for Rich only
    txt <- TG.liftRun $ scSummaryM sc True
    pure $ TG.toGVal txt


-- | fetch the table of contents for the page.
-- The argument is the level as an Integer which determines how many levels to
-- return in the HTML fragment.
tocDynamic
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SC.SourceContext
    -> TG.Function (RunSem r)
tocDynamic sc args = do
    let mLevels = tryExtractIntArg args
    txt <- TG.liftRun $ scTocM sc mLevels
    pure $ TG.toGVal txt


tryExtractIntArg :: Monad m => [(Maybe Text, TG.GVal m)] -> Maybe Int
tryExtractIntArg [] = Nothing
tryExtractIntArg xs =
    let xs' = filter (isNothing . fst) xs
     in case xs' of
         []        -> Nothing
         ((_,v):_) -> TG.toInt v
