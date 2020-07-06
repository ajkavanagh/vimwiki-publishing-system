{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Effect.Print
      where

import TextShow

import Control.Monad.IO.Class       (MonadIO, liftIO)

import           Prelude             hiding (print, putStrLn)
import qualified Prelude             as P
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Polysemy            (Embed, embed, Member, Sem, interpret, makeSem, runFinal, embedToFinal, Final)


data Print m a where
    PutText :: Text -> Print m ()
    Print :: (Show t) => t -> Print m ()


makeSem ''Print



printToIO :: ( MonadIO m
             , Member (Embed m) r
             )
          => Sem (Print ': r) a
          -> Sem r a
printToIO  = interpret $ \case
    PutText t -> embed $ liftIO $ P.putStrLn (T.unpack t)
    Print t -> embed $ liftIO $ P.print t
