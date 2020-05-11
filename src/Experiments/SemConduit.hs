{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}



module Experiments.SemConduit
      where


{- |
See if we can do something with Sem r and Conduit at the same time.

i.e. we have the @Sem r@ monad which has all of the effects that we want,
but we want to chunk files (or just lists of files, etc.) so that we don't
put everything in memory at the same time.  Obviously, we'll still have to
have a list of the SourcePageContext objects, but not hold the entire set of
files in memory at the same time.

This module is to explore combining Polysemy and Conduit into the same functions.

So Conduit looks like:

type Source m a = ConduitM () a m () -- no meaningful input or return value
type Conduit a m b = ConduitM a b m () -- no meaningful return value
type Sink a m b = ConduitM a Void m b -- no meaningful output value

from: Data.Conduit.Internal.Conduit

Core datatype of the conduit package. This type represents a general
component which can consume a stream of input values @i@, produce a stream
of output values @o@, perform actions in the @m@ monad, and produce a final
result @r@. The type synonyms provided here are simply wrappers around this
type.

newtype ConduitM i o m r = ConduitM
    { unConduitM :: forall b.
                    (r -> Pipe i i o () m b) -> Pipe i i o () m b
    }

-}

import           Data.Function     ((&))

import           Data.Conduit
import qualified Data.Conduit.List as CL

import           Colog.Core        (logStringStdout)
import           Colog.Polysemy    (Log, runLogAction)
import qualified Colog.Polysemy    as CP
import           Polysemy          (Embed, Members, Sem, embed, run)
import           Polysemy.Output   (runOutputList)

--
-- type Source m o = ConduitT () o m ()
{-source :: Source IO Int -- produces a stream of Ints-}
source :: ConduitT () Int IO ()  -- produces a stream of Ints
source = CL.sourceList [1..4]

-- type Sink i = ConduitT i Void
{-sink :: Sink String IO () -- consumes a stream of Strings, no result-}
sink :: ConduitT String Void IO () -- consumes a stream of Strings, no result
sink = CL.mapM_ putStrLn

-- type Conduit i m o = ConduitT i o m ()
{-conduit :: Conduit Int IO String -- converts Ints into Strings-}
conduit :: ConduitT Int String IO () -- converts Ints into Strings
conduit = CL.map show

main :: IO ()
main = runConduit $ source .| conduit .| sink


-- now let's try it with polysemy

sourceP :: ConduitT () Int (Sem r) ()
sourceP = CL.sourceList [1..4]


-- note, this is the only function that needs the Members as it uses CP.log
sinkP :: Members '[ Log String ] r
      => ConduitT String Void (Sem r) ()
sinkP = CL.mapM_ (CP.log @String)

conduitP :: ConduitT Int String (Sem r) ()
conduitP = CL.map show


mainP :: Members '[ Log String ] r
      => Sem r ()
mainP = runConduit $ sourceP .| conduitP .| sinkP


-- we run this as pure as the 'run' function resolves to a pure function
runMainP :: ([String], ())
runMainP = mainP
        & CP.runLogAsOutput  @String
        & runOutputList
        & run
