{-# LANGUAGE OverloadedStrings #-}

module Lib.RouteUtils
    ( checkDuplicateRoutes
    ) where

import qualified Data.List  as L
import Data.Ord (comparing)
import qualified Data.Text as T


import           Lib.Errors (SiteGenError (..))
import           Lib.Header (SourcePageHeader(..))


-- | check for duplicate routes in [SourcePageHeader]
-- If duplicate then throw an error against the 2nd SPH with details of
-- the error
checkDuplicateRoutes :: [SourcePageHeader] -> [SiteGenError]
checkDuplicateRoutes sphs =
    let pairs = map (\sph -> (phRoute sph, sph)) sphs
        sPairs = L.sortOn fst pairs
        gPairs = L.groupBy comp sPairs
        ePairs = L.filter ((>1).length) gPairs     -- [[p1,p2],[..],..]
     in concatMap makeError' ePairs
  where
    comp :: Eq a => (a,b) -> (a,b) -> Bool
    comp (x1,_) (x2,_) = x1 == x2


makeError' :: [(String, SourcePageHeader)] -> [SiteGenError]
makeError' pairs =
    let route = fst $ head pairs
        fileNames = map (phRelFilePath . snd) pairs
        sphs = map snd pairs
     in map (go route fileNames) sphs
  where
      go :: String -> [FilePath] -> SourcePageHeader -> SiteGenError
      go r fps sph = PageError sph (T.pack ("Pages share same route: "
                                         <> show r
                                         <> ", filenames: "
                                         <> L.intercalate ", " fps))

