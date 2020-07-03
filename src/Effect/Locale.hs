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

module Effect.Locale
      where

import           TextShow

import           Data.Text          (Text)

import           System.IO.Error    (tryIOError)

import           System.Locale.Read (TimeLocale)
import qualified System.Locale.Read as SLR


import           Polysemy           (Embed, Member, Members, Sem, embed,
                                     interpret, makeSem)
import           Polysemy.Error     (Error)
import qualified Polysemy.Error     as PE


data LocaleException = LocaleException (Maybe String) Text


instance Show LocaleException where
    show (LocaleException mTxt s) = "Error Getting Locale: "
                                 ++ "arg: " ++ show mTxt
                                 ++ "error: " ++ show s


data Locale m a where
    GetLocale :: Maybe String -> Locale m TimeLocale



makeSem ''Locale



localeToIO
    :: Members '[ Error LocaleException
                , Embed IO
                ] r
    => Sem (Locale ': r) a
    -> Sem r a
localeToIO = interpret $ \case
    -- Get the Locale for an optional text string
    -- GetLocale :: Maybe String -> Locale m TimeLocale
    GetLocale mtxt ->
        throwIfException mtxt =<< embed (tryIOError $ SLR.getLocale mtxt)



throwIfException
    :: ( Member (Error LocaleException) r
       , TextShow a
       )
    => Maybe String
    -> Either a b
    -> Sem r b
throwIfException mtxt e = case e of
    Left a -> PE.throw $ LocaleException mtxt (showt a)
    Right b -> pure b
