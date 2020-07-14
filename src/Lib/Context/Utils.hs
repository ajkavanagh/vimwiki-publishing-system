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

-- needed for the instance ToGVal m a instances that use the RunSem r monad
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.Utils where

import           Data.Default             (def)
import qualified Data.List                as L
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Text.Ginger              ((~>))
import qualified Text.Ginger              as TG

import           Colog.Polysemy           (Log)
import qualified Colog.Polysemy           as CP
import           Polysemy                 (Sem)
import qualified Polysemy.Reader          as PR

import           Effect.Ginger            (GingerSemEffects,
                                           GingerSemEffectsToGVal)
import qualified Effect.Logging           as EL

import           Lib.Context.Core         (contextFromList, tryExtractStringArg)
import           Types.Constants
import           Types.Context            (Context, GingerFunctionArgs, RunSem,
                                           RunSemGVal)
import           Types.Errors             (SiteGenError)
import           Types.Header             (SourceMetadata (..))
import           Types.SiteGenState       (SiteGenReader (..))

import           Lib.SpecialPages.Tag     (ensureTagPageFor, getAllTags,
                                           pagesForTag)

-- TODO: needed for the instance on TG.ToGVal SourceMetadata; perhaps we should move
-- it?
import           Lib.Context.PageContexts ()



-- | try to extract a string argument from the args list, and if successful, run
-- the supplied function against it return the require GVal in the RunSem r
-- monad.
stringArgFuncGValF
     :: GingerSemEffects r
     => (String -> RunSemGVal r)
     -> TG.Function (RunSem r)
stringArgFuncGValF f args =
    case tryExtractStringArg args of
        Nothing -> do
            TG.liftRun $ EL.logError "No text arg passed passed to function?"
            pure def
        Just s -> f (T.unpack s)


-- | Using the filterSourceMetadataItemsUsing function, but return the Ginger
-- context variable form of a (GVAl) list of (GVal) page items.
pagesForFunctionF
    :: GingerSemEffects r
    => (String -> [SourceMetadata] -> [SourceMetadata])
    -> String
    -> RunSemGVal r
pagesForFunctionF f s = do
    spcs <- TG.liftRun $ filterSourceMetadataItemsUsing f s
    pure $ TG.list $ map TG.toGVal spcs


-- | using a filter function and a input to that filter function, filter the
-- pages that the user supplied and return them in the Sem r monad
-- NOTE: it looks a bit wierd to supply both the function and one of it's
-- arguments, but this is so that we can curry this function to a function that
-- takes a string and returns the filtered source contexts.
filterSourceMetadataItemsUsing
    :: GingerSemEffects r
    => (String -> [SourceMetadata] -> [SourceMetadata])
    -> String
    -> Sem r [SourceMetadata]
filterSourceMetadataItemsUsing f s = do
    scs <- PR.asks @SiteGenReader siteSourceMetadataItems
    pure $  f s scs


-- | Extract the unamed string arg, if it exists, and return it as a String in
-- the Sem r monad
-- TODO: maybe remove this as it's hard to write the type sigs against this
-- function when composing.
stringArgRunSemR
    :: GingerSemEffects r
    => GingerFunctionArgs (Sem r)
    -> Sem r (Maybe String)
stringArgRunSemR args =
    case tryExtractStringArg args of
        Nothing -> do
            EL.logError "No text arg passed passed to function?"
            pure Nothing
        Just s -> pure $ Just (T.unpack s)


toTGlistGValm :: (Monad m, TG.ToGVal m a) => [a] -> TG.GVal m
toTGlistGValm xs = TG.list $ map TG.toGVal xs
