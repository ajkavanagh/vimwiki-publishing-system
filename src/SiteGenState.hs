module SiteGenState
    ( SiteGenReader(..)
    , makeSiteGenReader
    )
    where


import qualified Data.HashMap.Strict as HashMap

import           Lib.Header          (SourcePageHeader (..))
import           Lib.SiteGenConfig   (SiteGenConfig)


type Route = String


type FilePathToSPH = HashMap.HashMap FilePath SourcePageHeader
type RouteToSPH    = HashMap.HashMap Route SourcePageHeader


-- This is for the Reader which the
data SiteGenReader = SiteGenReader
    { siteGenConfig         :: !SiteGenConfig
    , siteSourcePageHeaders :: ![SourcePageHeader]
    , siteFilePathMap       :: !FilePathToSPH
    , siteRouteMap          :: !RouteToSPH
    }


instance Show SiteGenReader where
    show _ = "SiteGenReader<..>"


makeSiteGenReader :: SiteGenConfig -> [SourcePageHeader] -> SiteGenReader
makeSiteGenReader sgc sphs = SiteGenReader
    { siteGenConfig=sgc
    , siteSourcePageHeaders=sphs
    , siteFilePathMap=HashMap.fromList $ map (\h -> (phFileName h, h)) sphs
    , siteRouteMap=HashMap.fromList $ map (\h -> (phRoute h, h)) sphs
    }


-- | The State variable, that says where we ae at.
-- The important things are:
--  * it has the 'current' SourcePageHeader that is being worked on.
--  * There is a list of SiteGenErrors have been generated during the generation
--    so far.

