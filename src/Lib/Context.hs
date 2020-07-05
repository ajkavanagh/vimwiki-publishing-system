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

module Lib.Context where

import           Data.Text                   (Text)

import           Colog.Polysemy              (Log)
import qualified Colog.Polysemy              as CP
import           Polysemy                    (Member, Sem)
import           Polysemy.Error              (Error)
import           Polysemy.Reader             (Reader)
import           Polysemy.State              (State)
import           Polysemy.Writer             (Writer)

import           Effect.File                 (File, FileException)
import           Effect.Ginger               (GingerSemEffects)

import           Lib.Context.Core            (mergeContexts)
import           Lib.Context.DynamicContexts (pageFunctionsContext)
import           Lib.Context.Functions       (functionsContext)
import           Lib.Context.PageContexts    (pageHeaderContextFor)
import           Lib.Context.SiteGenConfig   (siteGenConfigContext)
import           Lib.Errors                  (GingerException (..),
                                              SiteGenError)
import           Lib.Header                  (SourceContext)
import qualified Lib.Header                  as H
import           Lib.SiteGenConfig           (SiteGenConfig)
import           Lib.SiteGenState            (SiteGenReader, SiteGenState)

import           Types.Context               (Context, RunSem, RunSemGVal)


makeContextFor
    :: GingerSemEffects r
    => SourceContext
    -> Sem r (Context (RunSem (Writer Text : r)))
makeContextFor sc = do
    CP.log @String $ "makeContextFor: " <> show (H.scRoute sc)
    pure $ mergeContexts [ pageHeaderContextFor sc
                         , siteGenConfigContext
                         , pageFunctionsContext sc
                         , functionsContext
                         ]


