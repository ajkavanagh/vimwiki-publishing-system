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

module Effect.Logging where

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Colog.Core.Severity (pattern D, pattern E, pattern I, Severity,
                                      pattern W, filterBySeverity)

import           Colog.Core.Action   (LogAction (..))
import           Colog.Message       (showSeverity)
import           Colog.Polysemy      (Log)
import qualified Colog.Polysemy      as CP
import           Polysemy            (Member, Sem)


data LoggingMessage = LM Severity (Maybe Text) Text
                      deriving Eq


extractSeverity :: LoggingMessage -> Severity
extractSeverity (LM sev _ _) = sev


instance Show LoggingMessage where
    show (LM sev (Just mod) txt) = T.unpack $ showSeverity sev <> " (" <> mod <> ") " <> txt
    show (LM sev Nothing    txt) = T.unpack $ showSeverity sev <> " " <> txt


makeLogger
    :: Member (Log LoggingMessage) r
    => Maybe Text -> Severity -> Text -> Sem r ()
makeLogger mod sev txt = CP.log @LoggingMessage (LM sev mod txt)


aLogger :: Member (Log LoggingMessage) r => Text -> Sem r ()
aLogger = makeLogger (Just "mymod") W


logDebug :: Member (Log LoggingMessage) r => Text -> Sem r ()
logDebug = makeLogger Nothing D


logWarning :: Member (Log LoggingMessage) r => Text -> Sem r ()
logWarning = makeLogger Nothing W


logError :: Member (Log LoggingMessage) r => Text -> Sem r ()
logError = makeLogger Nothing E


logInfo :: Member (Log LoggingMessage) r => Text -> Sem r ()
logInfo = makeLogger Nothing I


logAction :: LogAction IO LoggingMessage
logAction = LogAction print


logActionLevel :: Severity -> LogAction IO LoggingMessage
logActionLevel sev = filterBySeverity sev extractSeverity logAction
