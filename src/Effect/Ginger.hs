{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
--{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
--{-# LANGUAGE TemplateHaskell      #-}
--{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Effect.Ginger
      where

import TextShow

import           Data.Text          (Text)

import           Polysemy           (Embed, Member, Members, Sem, embed,
                                     embedToFinal, interpret, makeSem, run,
                                     runFinal)
import           Polysemy.Error     (Error)
import qualified Polysemy.Error     as PE


newtype GingerException = GingerException Text

instance Show GingerException where
    show ex = "Ginger Exception issue: " ++ ss
      where
          ss = case ex of
              (GingerException s)   -> show s



-- data Ginger m a where

