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
import qualified Data.List            as L
import           Data.Maybe           (isNothing)
import           Data.Text            (Text)
import qualified Data.Text            as T

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
import           Effect.Logging       (LoggingMessage)
import qualified Effect.Logging       as EL

import           Types.Context

import           Lib.Errors           (GingerException (..))
import qualified Lib.SiteGenConfig    as S

----

-- The @Context m@ is the Ginger Context support type that is used to shuffle
-- Sem r monad usage into the Ginger Run monad.  i.e. it's a way for Sem r code
-- to run the Ginger code and access the Sem r monad from within.
--
-- We use this so that we can run late parsing and processing of the Pandoc
-- markdown such that it is used when needed, rather than doing it all up front.

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
-- TODO: Add caching of the lookup function so that the result returned is
-- provided after the first generation of the key.  i.e. we only need the
-- dynamic lookup function to actually run once per invocation, not every time
-- it runs.  Perhaps add a wrapper for a memoised context?
contextLookup
    :: forall r.
       ( Member (Log String) r
       , Member (Log LoggingMessage) r
       )
    => Context (RunSem r)
    -> Text
    -> RunSemGVal r
contextLookup ctxt key =
    case resolveContext ctxt key of
        Nothing -> do
            TG.liftRun $ EL.logWarning $ "contextLookup for key: " <> key <> " was not resolved!"
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


extractBoolArg
    :: [(Maybe Text, TG.GVal m)]     -- ^ the args provided by Ginger
    -> Bool                          -- ^ the return value
extractBoolArg [] = False
extractBoolArg xs =
    let xs' = filter (isNothing . fst) xs
     in case xs' of
         []        -> False
         ((_,v):_) -> TG.asBoolean v


tryExtractListArg
    :: [(Maybe Text, TG.GVal m)]    -- ^ the args provided by Ginger
    -> Maybe [TG.GVal m]            -- ^ a Maybe List of the first args
tryExtractListArg [] = Nothing
tryExtractListArg xs =
    let xs' = filter (isNothing . fst) xs
     in case xs' of
         []         -> Nothing
         ((_, v):_) -> TG.asList v
