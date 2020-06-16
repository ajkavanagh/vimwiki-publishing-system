module Lib.SiteGenState
    ( SiteGenReader(..)
    , SiteGenState(..)
    , SourcePageContext(..)
    , SiteGenConfig(..)
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
import qualified Lib.Header          as H
import           Lib.SiteGenConfig   (SiteGenConfig)

import           Types.SiteGenState  (SiteGenReader (..), SiteGenState (..))


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
        }


emptySiteGenState :: SiteGenState
emptySiteGenState = def SiteGenState
