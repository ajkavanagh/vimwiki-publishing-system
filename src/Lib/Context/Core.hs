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

module Lib.Context.Core where

import qualified Data.ByteString.UTF8 as DBU
import           Data.Default         (def)
import           Data.Function        ((&))
import           Data.Hashable        (Hashable)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.List            as L
import           Data.Maybe           (isNothing)

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

import           Text.Ginger          (GVal, IncludeResolver, Source,
                                       SourceName, SourcePos, Template, ToGVal,
                                       (~>))
import qualified Text.Ginger          as TG
import           Text.Ginger.Html     (Html, htmlSource)

import           Effect.File          (File, FileException)
import qualified Effect.File          as EF

import           Effect.Ginger        (GingerException (..))

import qualified Lib.SiteGenConfig    as S

import           Experiments.Ginger   (parseToTemplate)

----

-- The @Context m@ is the Ginger Context support type that is used to shuffle
-- Sem r monad usage into the Ginger Run monad.  i.e. it's a way for Sem r code
-- to run the Ginger code and access the Sem r monad from within.
--
-- We use this so that we can run late parsing and processing of the Pandoc
-- markdown such that it is used when needed, rather than doing it all up front.

-- This is the type that Ginger runs for Sem r when doing context lookups.
type RunSem r = TG.Run TG.SourcePos (Sem r) Html
type RunSemGVal r = TG.Run TG.SourcePos (Sem r) Html (GVal (TG.Run TG.SourcePos (Sem r) Html))

-- The @Context m@ is basically a HashMap of Text to a function that will run
-- in the Ginger @Run@ monad, that returns a @GVal m@ where @m@ is going to be
-- the @Sem r@ monad, but that it's self will be wrapped inside the @Run@ monad.
newtype Context m = Context { unContext :: Monad m => HashMap.HashMap Text (m (GVal m)) }

emptyContext :: Monad m => Context m
emptyContext = Context HashMap.empty

-- resolve the @Context m@ and a key (@Text@) to a Maybe function that runs
-- in a Monad @m@ context and returns a @GVal m@.  i.e. we want to get back the
-- function that is stored in the hashmap via a key.
resolveContext :: Monad m => Context m -> Text -> Maybe (m (GVal m))
resolveContext ctxt k = HashMap.lookup k $ unContext ctxt


-- store a function that is used to resolve to a GVal m against a Text key
registerContextKeyGVal :: Monad m => Context m -> Text -> m (GVal m) -> Context m
registerContextKeyGVal ctxt key f = Context $ HashMap.insert key f (unContext ctxt)


-- store a list of (Text, m (GVal m)) as a new context
contextFromList :: Monad m => [(Text, m (GVal m))] -> Context m
contextFromList = Context . HashMap.fromList


-- update a context from a list of (Text, m (Gval m)) pairs as a new context
-- Note that the supplied context overwrites the existing keys
updateContextFromList :: Monad m => Context m -> [(Text, m (GVal m))] -> Context m
updateContextFromList ctxt cs = Context $ HashMap.fromList cs `HashMap.union` unContext ctxt


-- merge several sets of contexts; note that later contexts in the list 'win' in
-- terms of which key, function is kept.
mergeContexts :: Monad m => [Context m] -> Context m
mergeContexts = Context . HashMap.unions . L.reverse . map unContext


-- lookup the context value in the Context m hashmap.  The HashMap is in the
-- (Sem r) monad, but this function can run in the (Run p m h) monad which can
-- be built in to the renderer.  @Run p (Sem r) h@ is a @RunSemGval r@.
contextLookup
    :: forall r.
       ( Member (Log String) r
       )
    => Context (RunSem r)
    -> Text
    -> RunSemGVal r
contextLookup ctxt key =
    case resolveContext ctxt key of
        Nothing -> do
            TG.liftRun $ CP.log @String $ "contextLookup for key: " ++ T.unpack key ++ " was not resolved!"
            pure def
        Just f -> f


tryExtractIntArg :: Monad m => [(Maybe Text, TG.GVal m)] -> Maybe Int
tryExtractIntArg [] = Nothing
tryExtractIntArg xs =
    let xs' = filter (isNothing . fst) xs
     in case xs' of
         []        -> Nothing
         ((_,v):_) -> TG.toInt v


tryExtractStringArg :: Monad m => [(Maybe Text, TG.GVal m)] -> Maybe Text
tryExtractStringArg [] = Nothing
tryExtractStringArg xs =
    let xs' = filter (isNothing . fst) xs
     in case xs' of
         []        -> Nothing
         ((_,v):_) -> TG.fromGVal v
