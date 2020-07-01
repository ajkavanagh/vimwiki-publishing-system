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
    , nextSCToRender
    , addToRenderList
    )
    where


import           Control.Monad       (unless)

import           Data.Default.Class  (Default, def)
import           Data.DList          (DList)
import qualified Data.DList          as DList
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HashSet
import qualified Data.List           as L
import           Data.Ord            (comparing)

import           Polysemy            (Member, Sem)
import           Polysemy.State      (State)
import qualified Polysemy.State      as PS

import           Lib.Errors          (SiteGenError)
import           Lib.Header          (SourceContext (..),
                                      SourcePageContext (..))
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
        , siteRenderList=def
        , sitePagerSet=HashMap.empty
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


-- | nextSCToRender -- return the next item to render and remove it from the
-- list
nextSCToRender
    :: Member (State SiteGenState) r
    => Sem r (Maybe SourceContext)
nextSCToRender = do
    rs <- PS.gets @SiteGenState siteRenderList
    case rs of
        [] -> pure Nothing
        (r:rs') -> do
            PS.modify' $ \sgs -> sgs {siteRenderList=rs'}
            pure $ Just r


-- | addToRenderList - add SourceContext's to the render list.  Any duplicates
-- are dropped (which would be odd if there were), and the new items are
-- inserted in order.
addToRenderList
    :: Member (State SiteGenState) r
    => [SourceContext]
    -> Sem r ()
addToRenderList scs = case scs of
    [] -> pure ()
    scs' -> PS.modify' $ \sgs ->
        sgs {siteRenderList=insertListBy H.scRoute (siteRenderList sgs) scs}


-- | assuming the first list is sorted, insert the 2nd list into it, dropping
-- any equal elements.  The function is used as the accessor to sort the objects
-- on.
insertListBy :: Ord b => (a -> b) -> [a] -> [a] -> [a]
-- now use the fold and go to insert the items
insertListBy f = L.foldr go
  where
    go a as = insert1 f [] as a


insert1 :: Ord b => (a -> b) -> [a] -> [a] -> a -> [a]
insert1 _ ds [] x = reverse (x:ds)
insert1 f ds as'@(a:as) x = case comparing f a x of
        EQ -> reverse ds ++ as'      -- equal, then it already exists, so drop it
        LT -> insert1 f (a:ds) as x           -- LT, then keep searching for the insert
        GT -> reverse (x:ds) ++ as'  -- GT, just return the rest of the list
