{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Experiments.Errors where

import qualified Data.ByteString.UTF8 as DBU
import           Data.Default         (def)
import           Data.Function        ((&))
import           Data.Hashable        (Hashable)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Monad        (join)

import           Colog.Core           (logStringStderr)
import           Colog.Polysemy       (Log, runLogAction)
import qualified Colog.Polysemy       as CP
import           Polysemy             (Embed, Member, Members, Sem, embed,
                                       embedToFinal, interpret, makeSem, run,
                                       runFinal)
import           Polysemy.Error       (Error)
import qualified Polysemy.Error       as PE
import           Polysemy.Reader      (Reader)
import qualified Polysemy.Reader      as PR
import           Polysemy.Writer      (Writer)
import qualified Polysemy.Writer      as PW

import           Effect.File       (FileException(..))
import           Effect.Ginger     (GingerException(..))
import           Lib.SiteGenConfig (ConfigException(..))
import           Lib.Errors        (mapSiteGenError)

-- Experiment with throwing one type of error and catching another.

testThrow
    :: Members '[ Error FileException
                , Error GingerException
                ] r
    => Bool
    -> Sem r ()
testThrow False = PE.throw $ FileException "somepath" "Groovy error"
testThrow True  = PE.throw $ GingerException "a ginger exception, so far"



-- Run the Sem r monad to IO
runTest x = x & PE.mapError @GingerException mapSiteGenError
              & PE.mapError @FileException mapSiteGenError
              & PE.errorToIOFinal
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal



-- something I can just call
runThrow f = testThrow f & runTest
