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

{-# LANGUAGE PatternSynonyms       #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Experiments.Logging where

import           Data.Text           (Text)

import           Colog.Core.Severity (pattern D, pattern E, pattern I, Severity,
                                      pattern W, filterBySeverity)

import           Colog.Core          (LogAction, logStringStderr)
import           Colog.Core.Action   (LogAction (..))
import           Colog.Message       (showSeverity)
import           Colog.Polysemy      (Log, runLogAction)
import qualified Colog.Polysemy      as CP
import           Polysemy            (Embed, Final, Member, Members, Sem, embed,
                                      embedToFinal, interpret, makeSem, run,
                                      runFinal)

import           Effect.Logging


logTest :: Member (Log LoggingMessage) r
        => Sem r ()
logTest = do
    logDebug "Debug Message"
    logWarning "Warning Message"
    logError "Error Message"
    logInfo "Info Message"


runTest :: Sem '[Log LoggingMessage, Embed IO, Final IO] () -> IO ()
runTest f = runFinal @IO
          $ embedToFinal @IO
          $ runLogAction @IO (logActionLevel E) f
