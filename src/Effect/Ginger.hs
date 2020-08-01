{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Effect.Ginger
      where

import           TextShow

import           Data.Text          (Text)


import           Colog.Polysemy     (Log)
import           Polysemy           (Member, Sem)
import           Polysemy.Error     (Error)
import           Polysemy.Reader    (Reader)
import           Polysemy.State     (State)
import           Polysemy.Writer    (Writer)

import           Text.Ginger        (ToGVal)
import           Text.Pandoc        (Pandoc)

import           Effect.Cache       (Cache)
import           Effect.File        (File, FileException)
import           Effect.Locale      (Locale)
import           Effect.Logging     (LoggingMessage)
import           Effect.Print       (Print)

import           Types.Errors       (SiteGenError)
import           Types.SiteGenState (SiteGenReader, SiteGenState)

import           Lib.SiteGenConfig  (ConfigException, SiteGenConfig)


-- | This the effects bundle that all @RunSem r@ functions can expect in the
-- @Sem r@ monad.  They ar standardised here, so that we can maintain them in
-- once place.
type GingerSemEffects r
  =    ( Member File r
       , Member Locale r
       , Member (Cache Pandoc) r
       , Member (Cache Int) r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error FileException) r
       , Member (Error ConfigException) r
       , Member (Log LoggingMessage) r
       , Member Print r
       )


-- TODO: maybe remove this, as getting the types lined up is really tricky when
-- writing the type signature of a funciton.
type GingerSemEffectsToGVal r a
  =    ( Member File r
       , Member Locale r
       , Member (Cache Pandoc) r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Error FileException) r
       , Member (Error ConfigException) r
       , Member (Log LoggingMessage) r
       , Member Print r
       , ToGVal (Sem r) a
       )
