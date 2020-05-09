module SiteGenState
    ( SiteGenReader(..)
    , SiteGenState(..)
    , makeSiteGenReader
    , emptySiteGenState
    )
    where


import qualified Data.HashMap.Strict as HashMap
import           Data.DList          (DList)
import qualified Data.DList          as DList
import           Data.Default.Class  (Default, def)

import           Lib.Header          (SourcePageHeader (..))
import           Lib.SiteGenConfig   (SiteGenConfig)
import           Lib.Errors          (SiteGenError)


type Route = String


type FilePathToSPH = HashMap.HashMap FilePath SourcePageHeader
type RouteToSPH    = HashMap.HashMap Route SourcePageHeader


-- This is for the Reader which the
data SiteGenReader = SiteGenReader
    { siteGenConfig         :: !SiteGenConfig
    , siteSourcePageHeaders :: ![SourcePageHeader]
    , siteFilePathMap       :: !FilePathToSPH
    , siteRouteMap          :: !RouteToSPH
    } deriving (Show)


makeSiteGenReader :: SiteGenConfig -> [SourcePageHeader] -> SiteGenReader
makeSiteGenReader sgc sphs = SiteGenReader
    { siteGenConfig=sgc
    , siteSourcePageHeaders=sphs
    , siteFilePathMap=HashMap.fromList $ map (\h -> (phAbsFilePath h, h)) sphs
    , siteRouteMap=HashMap.fromList $ map (\h -> (phRoute h, h)) sphs
    }


-- | The State variable, that says where we ae at.
-- The important things are:
--  * it has the 'current' SourcePageHeader that is being worked on.
--  * There is a list of SiteGenErrors have been generated during the generation
--    so far.

-- So we'll have a DList of errors, and the current page. This probably means a
-- State var for the app


data SiteGenState = SiteGenState
    { siteGenPage   :: !SourcePageHeader
    , siteGenErrors :: !(DList SiteGenError)
    } deriving (Show)


instance Default SiteGenState where
    def = SiteGenState
        { siteGenPage=def
        , siteGenErrors=DList.empty
        }


emptySiteGenState :: SiteGenState
emptySiteGenState = def SiteGenState
