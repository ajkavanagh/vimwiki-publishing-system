module Lib.SiteGenState
    ( SiteGenReader(..)
    , SiteGenState(..)
    , SourcePageContext(..)
    , SiteGenConfig(..)
    , FilePathToSC
    , RouteToSC
    , VimWikiLinkToSC
    , Route
    , VimWikiLink
    , makeSiteGenReader
    , emptySiteGenState
    )
    where


import           Data.Default.Class  (Default, def)
import           Data.DList          (DList)
import qualified Data.DList          as DList
import qualified Data.HashMap.Strict as HashMap

import           Lib.Errors          (SiteGenError)
import           Lib.Header          (SourcePageContext (..))
import           Lib.SiteGenConfig   (SiteGenConfig)

import           Lib.SourceClass     (SourceContext (..))
import qualified Lib.SourceClass     as SC


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


makeSiteGenReader :: [SourceContext] -> SiteGenReader
makeSiteGenReader scs = SiteGenReader
    { siteSourceContexts=scs
    , siteVimWikiLinkMap=HashMap.fromList $ map (\h -> (SC.scVimWikiLinkPath h, h)) scs
    , siteRouteMap=HashMap.fromList $ map (\h -> (SC.scRoute h, h)) scs
    }


-- | The State variable, that says where we ae at.
-- The important things are:
--  * it has the 'current' SourcePageContext that is being worked on.
--  * There is a list of SiteGenErrors have been generated during the generation
--    so far.

-- So we'll have a DList of errors, and the current page. This probably means a
-- State var for the app


data SiteGenState = SiteGenState
    { siteGenPage   :: !SourcePageContext
    , siteGenErrors :: !(DList SiteGenError)
    } deriving (Show)


instance Default SiteGenState where
    def = SiteGenState
        { siteGenPage=def
        , siteGenErrors=DList.empty
        }


emptySiteGenState :: SiteGenState
emptySiteGenState = def SiteGenState
