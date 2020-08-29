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

module Effect.Time
      where

import           TextShow

import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Clock as DTC

import           System.IO.Error (tryIOError)

import           Polysemy        (Embed, Member, Members, Sem, embed, interpret,
                                  makeSem)
import           Polysemy.Error  (Error)
import qualified Polysemy.Error  as PE

import           Types.Errors    (SiteGenError (..))



data Time m a where
    GetCurrentTime :: Time m UTCTime



makeSem ''Time


timeToIO
    :: Members '[ Error SiteGenError
                , Embed IO
                ] r
    => Sem (Time ': r) a
    -> Sem r a
timeToIO = interpret $ \case
    -- Get the current Time
    -- GetCurrentTime :: Time m UTCTime
    GetCurrentTime ->
        throwIfException =<< embed (tryIOError DTC.getCurrentTime)


throwIfException
    :: ( Member (Error SiteGenError) r
       , TextShow a
       )
    => Either a b
    -> Sem r b
throwIfException e = case e of
    Left a  -> PE.throw $ OtherError (showt a)
    Right b -> pure b
