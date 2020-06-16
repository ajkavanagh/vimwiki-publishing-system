module Types.SiteGenState where


import qualified Data.HashMap.Strict as HashMap
import           Data.DList          (DList)

import           Lib.Header          (SourceContext (..), SourcePageContext (..))
import           Lib.Errors          (SiteGenError)


type Route = String
type VimWikiLink = String


type FilePathToSC    = HashMap.HashMap FilePath SourceContext
type RouteToSC       = HashMap.HashMap Route SourceContext
type VimWikiLinkToSC = HashMap.HashMap VimWikiLink SourceContext



-- This is for the Reader which the
data SiteGenReader = SiteGenReader
    { siteSourceContexts :: ![SourceContext]
    , siteVimWikiLinkMap :: !VimWikiLinkToSC
    , siteRouteMap       :: !RouteToSC
    } deriving (Show)



data SiteGenState = SiteGenState
    { siteGenPage   :: !SourcePageContext
    , siteGenErrors :: !(DList SiteGenError)
    } deriving (Show)
