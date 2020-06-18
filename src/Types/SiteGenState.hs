{-# LANGUAGE DeriveGeneric #-}

module Types.SiteGenState where

import           GHC.Generics        (Generic)

import qualified Data.HashMap.Strict as HashMap
import           Data.DList          (DList)
import           Data.HashSet        (HashSet)
import           Data.Hashable       (Hashable)

import           Lib.Header          (SourceContext (..), SourcePageContext (..))
import           Lib.Errors          (SiteGenError)


type Route = String
type VimWikiLink = String


type FilePathToSC    = HashMap.HashMap FilePath SourceContext
type RouteToSC       = HashMap.HashMap Route SourceContext
type VimWikiLinkToSC = HashMap.HashMap VimWikiLink SourceContext


data FileMemo = FileMemo FilePath
              | DirMemo FilePath
              deriving (Eq, Show, Generic)


instance Hashable FileMemo


-- This is for the Reader which the
data SiteGenReader = SiteGenReader
    { siteSourceContexts :: ![SourceContext]
    , siteVimWikiLinkMap :: !VimWikiLinkToSC
    , siteRouteMap       :: !RouteToSC
    } deriving (Show)



data SiteGenState = SiteGenState
    { siteGenPage   :: !SourcePageContext
    , siteGenErrors :: !(DList SiteGenError)
    , memoFiles     :: !(HashSet FileMemo)
    } deriving (Show)
