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
type GingerFunctionArgs r = [(Maybe Text, GVal r)]

-- Also note that a TG.Function is:
-- type Function m = [(Maybe Text, GVal m)] -> m (GVal m)
-- so with a Function (RunSem r), the bit to the rgiht, is a RunSemGVal r

-- The @Context m@ is basically a HashMap of Text to a function that will run
-- in the Ginger @Run@ monad, that returns a @GVal m@ where @m@ is going to be
-- the @Sem r@ monad, but that it's self will be wrapped inside the @Run@ monad.
newtype Context m = Context { unContext :: Monad m => HashMap.HashMap Text (m (GVal m)) }



data ContextObject = SMObject String
                   | TagObject String
                   | CategoryObject String
                   deriving (Show, Eq)


-- these are for identifying higher object types when they are GVal m
-- For use in functions that need to work out what has been passed.
data ContextObjectTypes = SMObjectType
                        | TagObjectType
                        | CategoryObjectType
                        | PagerObjectType
                        | FeedItemObjectType
                        deriving (Show, Eq)


instance ToGVal m ContextObjectTypes where
    toGVal SMObjectType       = toGVal ":sm:"
    toGVal TagObjectType      = toGVal ":tag:"
    toGVal CategoryObjectType = toGVal ":category:"
    toGVal PagerObjectType    = toGVal ":pager:"
    toGVal FeedItemObjectType = toGVal ":feeditem:"


instance FromGVal m ContextObjectTypes where
    fromGVal g = lookupKey "_objectType_" g >>= \g' -> case asText g' of
            ""           -> Nothing
            ":sm:"       -> Just SMObjectType
            ":tag:"      -> Just TagObjectType
            ":cat:"      -> Just CategoryObjectType
            ":pager:"    -> Just PagerObjectType
            ":feeditem:" -> Just FeedItemObjectType


gValContextObjectTypeDictItemFor :: ContextObjectTypes -> Pair m
gValContextObjectTypeDictItemFor cot = "_objectType_" ~> cot
