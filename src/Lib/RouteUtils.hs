{-# LANGUAGE OverloadedStrings #-}

module Lib.RouteUtils
    ( checkDuplicateRoutes
    ) where

import qualified Data.List  as L
import Data.Ord (comparing)
import qualified Data.Text as T


import           Lib.Errors (SiteGenError (..))
import           Lib.Header (SourcePageContext(..))


-- | check for duplicate routes in [SourcePageContext]
-- If duplicate then throw an error against the 2nd SPC with details of
-- the error
checkDuplicateRoutes :: [SourcePageContext] -> [SiteGenError]
checkDuplicateRoutes spcs =
    let pairs = map (\spc -> (spcRoute spc, spc)) spcs
        sPairs = L.sortOn fst pairs
        gPairs = L.groupBy comp sPairs
        ePairs = L.filter ((>1).length) gPairs     -- [[p1,p2],[..],..]
     in concatMap makeError' ePairs
  where
    comp :: Eq a => (a,b) -> (a,b) -> Bool
    comp (x1,_) (x2,_) = x1 == x2


makeError' :: [(String, SourcePageContext)] -> [SiteGenError]
makeError' pairs =
    let route = fst $ head pairs
        fileNames = map (spcRelFilePath . snd) pairs
        spcs = map snd pairs
     in map (go route fileNames) spcs
  where
      go :: String -> [FilePath] -> SourcePageContext -> SiteGenError
      go r fps spc = PageError spc (T.pack ("Pages share same route: "
                                         <> show r
                                         <> ", filenames: "
                                         <> L.intercalate ", " fps))

