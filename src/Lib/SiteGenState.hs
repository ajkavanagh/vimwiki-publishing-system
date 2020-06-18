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

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.SiteGenState
    ( SiteGenReader(..)
    , SiteGenState(..)
    , SourcePageContext(..)
    , SiteGenConfig(..)
    , FileMemo(..)
    , makeSiteGenReader
    , emptySiteGenState
    , recordMemo
    )
    where


import           Control.Monad       (unless)

import           Data.Default.Class  (Default, def)
import           Data.DList          (DList)
import qualified Data.DList          as DList
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet

import           Polysemy            (Member, Sem)
import           Polysemy.State      (State)
import qualified Polysemy.State      as PS

import           Lib.Errors          (SiteGenError)
import           Lib.Header          (SourcePageContext (..))
import qualified Lib.Header          as H
import           Lib.SiteGenConfig   (SiteGenConfig)

import           Types.SiteGenState  (FileMemo (..), SiteGenReader (..),
                                      SiteGenState (..))


makeSiteGenReader :: [H.SourceContext] -> SiteGenReader
makeSiteGenReader scs = SiteGenReader
    { siteSourceContexts=scs
    , siteVimWikiLinkMap=HashMap.fromList $ map (\h -> (H.scVimWikiLinkPath h, h)) scs
    , siteRouteMap=HashMap.fromList $ map (\h -> (H.scRoute h, h)) scs
    }


instance Default SiteGenState where
    def = SiteGenState
        { siteGenPage=def
        , siteGenErrors=DList.empty
        , memoFiles=HashSet.empty
        }


emptySiteGenState :: SiteGenState
emptySiteGenState = def SiteGenState


-- | record a FileMemo in the hashset if it doesn't already exist
recordMemo
    :: Member (State SiteGenState) r
    => FileMemo
    -> Sem r ()
recordMemo memoFile = do
    memo <- PS.gets @SiteGenState memoFiles
    unless (memoFile `HashSet.member` memo) $
        PS.modify' $ \sgs -> sgs { memoFiles=HashSet.insert memoFile memo }
