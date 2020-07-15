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
    , SourceMetadata(..)
    , SiteGenConfig(..)
    , FileMemo(..)
    , makeSiteGenReader
    , emptySiteGenState
    , recordMemo
    , toFilePath
    , nextSMToRender
    , addToRenderList
    , addToSitePagesRendered
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

import           Lib.SiteGenConfig   (SiteGenConfig)
import           Types.Errors        (SiteGenError)
import           Types.Header        (SourceMetadata (..))

import           Types.SiteGenState  (FileMemo (..), SiteGenReader (..),
                                      SiteGenState (..))


makeSiteGenReader :: [SourceMetadata] -> SiteGenReader
makeSiteGenReader sms = SiteGenReader
    { siteSourceMetadataItems=sms
    , siteVimWikiLinkMap=HashMap.fromList $ map (\h -> (smVimWikiLinkPath h, h)) sms
    , siteRouteMap=HashMap.fromList $ map (\h -> (smRoute h, h)) sms
    }


instance Default SiteGenState where
    def = SiteGenState
        { siteGenPage=def
        , siteGenErrors=DList.empty
        , memoFiles=HashSet.empty
        , siteRenderList=def
        , sitePagerSet=HashMap.empty
        , sitePagesRendered=HashMap.empty
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


toFilePath :: FileMemo -> FilePath
toFilePath (FileMemo fp) = fp
toFilePath (DirMemo fp)  = fp


addToSitePagesRendered
    :: Member (State SiteGenState) r
    => SourceMetadata
    -> Sem r ()
addToSitePagesRendered sm =
    PS.modify' $ \sgs ->
        sgs { sitePagesRendered=HashMap.insert (smRoute sm)
                                               sm
                                               (sitePagesRendered sgs)}

-- | nextSCToRender -- return the next item to render and remove it from the
-- list
nextSMToRender
    :: Member (State SiteGenState) r
    => Sem r (Maybe SourceMetadata)
nextSMToRender = do
    rs <- PS.gets @SiteGenState siteRenderList
    case rs of
        [] -> pure Nothing
        (r:rs') -> do
            PS.modify' $ \sgs -> sgs {siteRenderList=rs'}
            pure $ Just r


-- | addToRenderList - add SourceMetadata items to the render list.  Any
-- duplicates are dropped (which would be odd if there were), and the new items
-- are inserted in order.
addToRenderList
    :: Member (State SiteGenState) r
    => [SourceMetadata]
    -> Sem r ()
addToRenderList sms = case sms of
    [] -> pure ()
    scs' -> PS.modify' $ \sgs ->
        sgs {siteRenderList=insertListBy smRoute (siteRenderList sgs) sms}


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
