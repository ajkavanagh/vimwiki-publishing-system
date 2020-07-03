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

import           Data.Text              (Text)


import           Colog.Polysemy         (Log)
import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import           Polysemy.Reader        (Reader)
import           Polysemy.State         (State)
import           Polysemy.Writer        (Writer)

import           Effect.ByteStringStore (ByteStringStore)
import           Effect.File            (File)

import           Types.SiteGenState     (SiteGenReader, SiteGenState)

import           Lib.Errors             (SiteGenError)
import           Lib.SiteGenConfig      (SiteGenConfig)


-- data Ginger m a where


-- | This the effects bundle that all @RunSem r@ functions can expect in the
-- @Sem r@ monad.  They ar standardised here, so that we can maintain them in
-- once place.
type GingerSemEffects r
  =    ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )


