{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Types.Context where


import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)

import           Text.Ginger         (FromGVal, GVal, Pair, Run, SourcePos,
                                      ToGVal, asText, lookupKey, toGVal, (~>))
import qualified Text.Ginger         as TG
import           Text.Ginger.Html    (Html)

import           Polysemy            (Sem)


----

-- The @Context m@ is the Ginger Context support type that is used to shuffle
-- Sem r monad usage into the Ginger Run monad.  i.e. it's a way for Sem r code
-- to run the Ginger code and access the Sem r monad from within.
--
-- We use this so that we can run late parsing and processing of the Pandoc
-- markdown such that it is used when needed, rather than doing it all up front.

-- This is the type that Ginger runs for Sem r when doing context lookups.
type RunSem r = Run SourcePos (Sem r) Html
type GValRunSem r = GVal (RunSem r)
type RunSemGVal r = Run SourcePos (Sem r) Html (GValRunSem r)

-- The @Context m@ is basically a HashMap of Text to a function that will run
-- in the Ginger @Run@ monad, that returns a @GVal m@ where @m@ is going to be
-- the @Sem r@ monad, but that it's self will be wrapped inside the @Run@ monad.
newtype Context m = Context { unContext :: Monad m => HashMap.HashMap Text (m (GVal m)) }



data ContextObject = SPCObject String
                   | VPCObject String
                   | TagObject String
                   | CategoryObject String
                   deriving (Show, Eq)


-- these are for identifying higher object types when they are GVal m
-- For use in functions that need to work out what has been passed.
data ContextObjectTypes = SPCObjectType
                        | VPCObjectType
                        | TagObjectType
                        | CategoryObjectType
                        | PagerObjectType
                        deriving (Show, Eq)


instance ToGVal m ContextObjectTypes where
    toGVal SPCObjectType      = toGVal ":spc:"
    toGVal VPCObjectType      = toGVal ":vpc:"
    toGVal TagObjectType      = toGVal ":tag:"
    toGVal CategoryObjectType = toGVal ":category:"
    toGVal PagerObjectType    = toGVal ":pager:"


instance FromGVal m ContextObjectTypes where
    fromGVal g = lookupKey "_objectType_" g >>= \g' -> case asText g' of
            ""        -> Nothing
            ":spc:"   -> Just SPCObjectType
            ":vpc:"   -> Just VPCObjectType
            ":tag:"   -> Just TagObjectType
            ":cat:"   -> Just CategoryObjectType
            ":pager:" -> Just PagerObjectType


gValContextObjectTypeDictItemFor :: ContextObjectTypes -> Pair m
gValContextObjectTypeDictItemFor cot = "_objectType_" ~> cot
