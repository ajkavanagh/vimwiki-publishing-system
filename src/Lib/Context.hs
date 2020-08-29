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

import           Data.Text                     (Text)

import           Polysemy                      (Sem)
import           Polysemy.Writer               (Writer)

import           Effect.Ginger                 (GingerSemEffects)

import           Lib.Context.CategoriesContext (categoriesContext)
import           Lib.Context.Core              (mergeContexts)
import           Lib.Context.DynamicContexts   (pageFunctionsContext)
import           Lib.Context.FeedContext       (feedContext)
import           Lib.Context.Functions         (functionsContext)
import           Lib.Context.PageContexts      (pageHeaderContextFor)
import           Lib.Context.SiteGenConfig     (siteGenConfigContext)
import           Lib.Context.TagsContext       (tagsContext)

import           Types.Context                 (Context, RunSem)
import           Types.Header                  (SourceMetadata)


makeContextFor
    :: GingerSemEffects r
    => SourceMetadata
    -> Sem r (Context (RunSem (Writer Text : r)))
makeContextFor sm =
    pure $ mergeContexts [ pageHeaderContextFor sm
                         , siteGenConfigContext
                         , pageFunctionsContext sm
                         , functionsContext
                         , categoriesContext sm
                         , tagsContext sm
                         , feedContext sm
                         ]
