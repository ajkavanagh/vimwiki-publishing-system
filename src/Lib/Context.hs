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

import qualified Data.ByteString.UTF8     as DBU
import           Data.Default             (def)
import           Data.Function            ((&))
import           Data.Hashable            (Hashable)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Colog.Core               (logStringStderr)
import           Colog.Polysemy           (Log, runLogAction)
import qualified Colog.Polysemy           as CP
import           Polysemy                 (Embed, Member, Members, Sem, embed,
                                           embedToFinal, interpret, makeSem,
                                           run, runFinal)
import           Polysemy.Error           (Error)
import qualified Polysemy.Error           as PE
import           Polysemy.Reader          (Reader)
import qualified Polysemy.Reader          as PR
import           Polysemy.State           (State)
import qualified Polysemy.State           as PS
import           Polysemy.Writer          (Writer)
import qualified Polysemy.Writer          as PW

import           Text.Ginger              (GVal, IncludeResolver, Source,
                                           SourceName, SourcePos, Template,
                                           ToGVal, (~>))
import qualified Text.Ginger              as TG
import           Text.Ginger.Html         (Html, htmlSource)

import           Effect.ByteStringStore   (ByteStringStore)
import qualified Effect.ByteStringStore   as EB
import           Effect.File              (File, FileException)
import qualified Effect.File              as EF

import           Effect.Ginger            (GingerException (..))

import           Lib.SiteGenConfig        (SiteGenConfig)
import qualified Lib.SiteGenConfig        as S

import           Experiments.Ginger       (parseToTemplate)

import           Lib.Context.Core         (Context, RunSem, RunSemGVal,
                                           contextFromList, mergeContexts)
import           Lib.Context.PageContexts (pageHeaderContextFor)
import           Lib.Context.SiteGenConfig (siteGenConfigContext)
import           Lib.Context.DynamicContexts (pageFunctionsContext)
import           Lib.Errors               (SiteGenError)
import           Lib.SiteGenState         (SiteGenReader, SiteGenState)
import qualified Lib.SourceClass          as SC


makeContextFor
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SC.SourceContext
    -> Sem r (Context (RunSem (Writer Text : r)))
makeContextFor sc = pure $ mergeContexts [ pageHeaderContextFor sc
                                         , siteGenConfigContext
                                         , pageFunctionsContext sc ]


