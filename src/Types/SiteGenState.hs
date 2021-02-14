{-# LANGUAGE DeriveGeneric #-}

module Types.SiteGenState where

import           GHC.Generics        (Generic)

import           Data.DList          (DList)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet        (HashSet)

import           Types.Constants
import           Types.Context       (ContextObject (..))
import           Types.Errors        (SiteGenError)
import           Types.Pager         (Pager, RouteToPager)

import           Types.Header        (SourceMetadata (..))


type FilePathToSM    = HashMap.HashMap FilePath SourceMetadata
type RouteToSM       = HashMap.HashMap Route SourceMetadata
type VimWikiLinkToSM = HashMap.HashMap VimWikiLink SourceMetadata

data FileMemo = FileMemo FilePath
              | DirMemo FilePath
              deriving (Eq, Show, Generic)


instance Hashable FileMemo


data SiteGenReader = SiteGenReader
    { siteSourceMetadataItems :: ![SourceMetadata]
    , siteVimWikiLinkMap      :: !VimWikiLinkToSM
    , siteFilePathToSM        :: !FilePathToSM
    , siteRouteMap            :: !RouteToSM
    } deriving (Show)


data SiteGenState = SiteGenState
    { siteGenPage       :: !SourceMetadata
    , siteGenErrors     :: !(DList SiteGenError)
    , memoFiles         :: !(HashSet FileMemo)
    , siteRenderList    :: ![SourceMetadata]    -- this is a sorted list of "next to render"
    , sitePagerSet      :: !RouteToPager
    , sitePagesRendered :: !RouteToSM
    } deriving (Show)
