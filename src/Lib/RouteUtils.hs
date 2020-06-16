{-# LANGUAGE OverloadedStrings #-}

module Lib.RouteUtils
    ( checkDuplicateRoutes
    , ensureIndexRoutesIn
    , indexRouteFor
    , isIndexRoute
    , findMissingIndexRoutes
    , addVPCIndexPages
    , RouteError(..)
    ) where

import           Data.Default.Class (Default, def)
import qualified Data.List          as L
import qualified Data.List.Split    as LS
import           Data.Ord           (comparing)
import qualified Data.Text          as T

import qualified System.FilePath    as F

import           Lib.Errors         (SiteGenError (..))
import           Lib.Header         (SourceContext (..), SourcePageContext (..),
                                     VirtualPageContext (..))


data RouteError = DuplicateRouteError SourcePageContext T.Text
                  deriving (Eq, Show)

-- | check for duplicate routes in [SourcePageContext]
-- If duplicate then throw an error against the 2nd SPC with details of
-- the error
checkDuplicateRoutes :: [SourcePageContext] -> [RouteError]
checkDuplicateRoutes spcs =
    let pairs = map (\spc -> (spcRoute spc, spc)) spcs
        sPairs = L.sortOn fst pairs
        gPairs = L.groupBy comp sPairs
        ePairs = L.filter ((>1).length) gPairs     -- [[p1,p2],[..],..]
     in concatMap makeError' ePairs
  where
    comp :: Eq a => (a,b) -> (a,b) -> Bool
    comp (x1,_) (x2,_) = x1 == x2


makeError' :: [(String, SourcePageContext)] -> [RouteError]
makeError' pairs =
    let route = fst $ head pairs
        fileNames = map (spcRelFilePath . snd) pairs
        spcs = map snd pairs
     in map (go route fileNames) spcs
  where
      go :: String -> [FilePath] -> SourcePageContext -> RouteError
      go r fps spc = DuplicateRouteError spc (T.pack ("Pages share same route: "
                                             <> show r
                                             <> ", filenames: "
                                             <> L.intercalate ", " fps))


-- | fix the index page routes in SourcePageContext records that are an index
-- page.
ensureIndexRoutesIn :: [SourcePageContext] -> [SourcePageContext]
ensureIndexRoutesIn spcs = go <$> spcs
  where go spc = let r = spcRoute spc
                  in if spcIndexPage spc && isIndexRoute r
                       then spc { spcRoute=r <> "/" }
                       else spc


-- | identify the missing index pages in the routes.
-- We are looking for routes that have a 'directories' in them for which no
-- index page exists at that point.
-- e.g. thing/one route needs a thing/ page.
findMissingIndexRoutes :: [SourcePageContext] -> [String]
findMissingIndexRoutes spcs =
    let allRoutes = L.nub $ L.sort $ fmap (indexRouteFor . spcRoute) spcs
        actualRoutes = spcRoute <$> filter spcIndexPage spcs
     in allRoutes L.\\ actualRoutes


isIndexRoute :: String -> Bool
isIndexRoute "" = False
isIndexRoute s  = last s == '/'


-- | compute the index route for a route.  if it ends in the path separator,
-- then it is an index route; otherwise take the last part off, and then that is
-- a route.
indexRouteFor :: String -> String
indexRouteFor "" = "/"
indexRouteFor s
  | last s == '/' = s
  | otherwise =
      case LS.splitOn "/" s of
          [] -> "/"
          xs -> L.intercalate "/" (L.init xs)  <> "/"


-- | ensure that we have a set of VirtualPageContext records for each missing
-- index page.  Note that SourceContext is a wrapper around the
-- SourcePageContext and the VirtualPageContext, and the SourceClass is used to
-- paper over the cracks in most places.
addVPCIndexPages :: [SourcePageContext] -> [SourceContext]
addVPCIndexPages spcs =
    let missingIndexes = findMissingIndexRoutes spcs
        vpcs = map makeVPCForIndex missingIndexes
     in map SPC spcs ++ map VPC vpcs


makeVPCForIndex :: String -> VirtualPageContext
makeVPCForIndex route = def { vpcRoute = route
                            , vpcVimWikiLinkPath = route
                            , vpcTitle = "Page: " ++ route
                            , vpcTemplate = "index"
                            , vpcIndexPage = True
                            }
