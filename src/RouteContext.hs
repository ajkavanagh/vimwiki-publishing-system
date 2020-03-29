module RouteContext where


import           Data.Time.Clock (UTCTime)


data RouteContext = RouteContext
    { autoSlug  :: String
    , fileTime  :: UTCTime
    , autoTitle :: String
    } deriving Show
