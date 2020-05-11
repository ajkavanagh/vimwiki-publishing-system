module Lib.SiteGenState
    ( SiteGenReader(..)
    , SiteGenState(..)
    , FilePathToSPC
    , RouteToSPC
    , VimWikiLinkToSPC
    , Route
    , VimWikiLink
    , makeSiteGenReader
    , emptySiteGenState
    )
    where


import qualified Data.HashMap.Strict as HashMap
import           Data.DList          (DList)
import qualified Data.DList          as DList
import           Data.Default.Class  (Default, def)

import           Lib.Header          (SourcePageContext (..))
import           Lib.SiteGenConfig   (SiteGenConfig)
import           Lib.Errors          (SiteGenError)


type Route = String
type VimWikiLink = String


type FilePathToSPC    = HashMap.HashMap FilePath SourcePageContext
type RouteToSPC       = HashMap.HashMap Route SourcePageContext
type VimWikiLinkToSPC = HashMap.HashMap VimWikiLink SourcePageContext


-- This is for the Reader which the
data SiteGenReader = SiteGenReader
    { siteGenConfig         :: !SiteGenConfig
    , siteSourcePageContexts :: ![SourcePageContext]
    , siteFilePathMap       :: !FilePathToSPC
    , siteVimWikiLinkMap    :: !VimWikiLinkToSPC
    , siteRouteMap          :: !RouteToSPC
    } deriving (Show)


makeSiteGenReader :: SiteGenConfig -> [SourcePageContext] -> SiteGenReader
makeSiteGenReader sgc spcs = SiteGenReader
    { siteGenConfig=sgc
    , siteSourcePageContexts=spcs
    , siteFilePathMap=HashMap.fromList $ map (\h -> (spcAbsFilePath h, h)) spcs
    , siteVimWikiLinkMap=HashMap.fromList $ map (\h -> (spcVimWikiLinkPath h, h)) spcs
    , siteRouteMap=HashMap.fromList $ map (\h -> (spcRoute h, h)) spcs
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
