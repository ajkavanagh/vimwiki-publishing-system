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

import qualified Data.HashMap.Strict       as HashMap
import           Data.Maybe                (isNothing)
import           Data.Text                 (Text, pack, toLower)

import           Text.Ginger               ((~>))
import qualified Text.Ginger               as TG
import qualified Text.Ginger.Html          as TGH
import qualified Text.Ginger.Run.FuncUtils as TF

import           Colog.Polysemy            (Log)
import qualified Colog.Polysemy            as CP
import           Polysemy                  (Member, Sem)
import           Polysemy.Error            (Error)
import           Polysemy.Reader           (Reader)
import           Polysemy.State            (State)
import           Polysemy.Writer           (Writer)

import           Effect.File               (File)
import           Effect.Ginger             (GingerSemEffects)
import qualified Effect.Logging            as EL

import           Types.Constants           (wordsPerMinute)
import           Types.Context             (Context, RunSem, RunSemGVal)
import           Types.Errors              (SiteGenError)
import           Types.Header              (SourceMetadata (..))

import           Lib.Context.Core          (contextFromList, tryExtractIntArg)
import           Lib.Pandoc                (smContentM, smSummaryM, smTocM,
                                            wordCountM)
import           Lib.SiteGenConfig         (SiteGenConfig)
import           Lib.SiteGenState          (SiteGenReader, SiteGenState)


-- Basically, this module provides the 'content', 'summary' and 'toc' html
-- fragments for a SourceMetadata.


pageFunctionsContext
    :: GingerSemEffects r
    => SourceMetadata
    -> Context (RunSem r)
pageFunctionsContext sm = contextFromList
    [ ("content",  funcDynamicMGValM contentDynamic sm)
    , ("summary", funcDynamicMGValM summaryDynamic sm)
    , ("toc", funcDynamicMGValM tocDynamic sm)
    , ("readingTime", funcDynamicMGValM readingTimeDynamic sm)
    , ("wordCount", funcDynamicMGValM wordCountDynamic sm)]


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
-- TODO: needs the plain=True version support as well.
contentDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
contentDynamic sm _ = do           -- content ignores the args
    --TG.liftRun $ EL.logDebug "Dynamic content was asked for"
    txt <- TG.liftRun $ smContentM sm
    pure $ TG.toGVal $ TGH.unsafeRawHtml txt


-- | fetch the summary of the page, as raw html, and whether it was truncated
-- TODO: match the args for plain=True to select for plain summary rather than
--       rich summary
summaryDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
summaryDynamic sm args = do   -- summary ignores the args, and selects for Rich only
    let isPlain = extractOptionalPlainArg args
    (txt, truncated) <- TG.liftRun $ smSummaryM sm (not isPlain)
    pure $ TG.dict ["Html"      ~> TGH.unsafeRawHtml txt
                   ,"Truncated" ~> truncated
                   ]


extractOptionalPlainArg
    :: [(Maybe Text, TG.GVal m)] -- ^ the args provided by Ginger
    -> Bool                      -- ^ whether plain is set, default is False
extractOptionalPlainArg args =
    let (posArgs, _, _, _) = TF.extractArgs ["plain"] args
     in maybe False (toBoolean . TG.asText) (HashMap.lookup "plain" posArgs)


toBoolean :: Text -> Bool
toBoolean xs = let lxs = toLower xs in case lxs of
    "false" -> False
    "0"     -> False
    ""      -> False
    _       -> True


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


-- | calcuate the reading time of a page based on wordsPerMinute in
-- Types.Constants.
readingTimeDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
readingTimeDynamic sm _ = do
    words <- TG.liftRun $ wordCountM sm
    pure $ TG.toGVal ((words `quot` wordsPerMinute) +1)


-- | Return the word count for the page.
wordCountDynamic
    :: GingerSemEffects r
    => SourceMetadata
    -> TG.Function (RunSem r)
wordCountDynamic sm _ = do
    words <- TG.liftRun $ wordCountM sm
    TG.liftRun $ EL.logDebug $ pack $ "------- word count: " ++ show words
    pure $ TG.toGVal words
