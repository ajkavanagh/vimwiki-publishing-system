{-# LANGUAGE OverloadedStrings #-}

module Lib.RouteUtils
    ( checkDuplicateRoutes
    , checkDuplicateRoutesSPC
    , ensureIndexRoutesIn
    , indexRouteFor
    , isIndexRoute
    , findMissingIndexRoutes
    , addVPCIndexPages
    , makeFileNameFrom
    , makeFileNoExtNameFrom
    , RouteError(..)
    ) where

import           Data.Default.Class (Default, def)
import           Data.Function      (on)
import qualified Data.List          as L
import qualified Data.List.Split    as LS
import           Data.Maybe         (fromMaybe)
import           Data.Ord           (comparing)
import qualified Data.Text          as T

import qualified System.FilePath    as F

import           Lib.Errors         (SiteGenError (..))
import           Lib.Header         (SourceContext (..), SourcePageContext (..),
                                     VirtualPageContext (..), scIndexPage,
                                     scRelFilePath, scRoute)


data RouteError = DuplicateRouteError SourceContext T.Text
                  deriving (Eq, Show)


-- | checks for duplicate routes in the SPCs, then generate any missing index
-- pages and finally check for inconsistent indexes (i.e. where an output
-- filename can't be generated due to index pages.
validateRoutesInSPCs
    :: Bool                                -- whether we are generating "indexPages"
    -> String                              -- The final extension ".html"
    -> [SourcePageContext]
    -> Either [RouteError] [SourceContext]
validateRoutesInSPCs doIndexFileNames ext spcs =
    let errors = checkDuplicateRoutesSPC spcs
     in if not (null errors)
          then Left errors
          else let spcs' = ensureIndexRoutesIn spcs
                   scs = addVPCIndexPages spcs'
                   ferrors = checkDuplicateFiles doIndexFileNames ext scs
                in if not (null ferrors)
                     then Left errors
                     else Right scs



-- | check for duplicate routes in [SourceContext]
checkDuplicateRoutes :: [SourceContext] -> [RouteError]
checkDuplicateRoutes = checkDuplicateUsing scRoute


checkDuplicateRoutesSPC :: [SourcePageContext] -> [RouteError]
checkDuplicateRoutesSPC = checkDuplicateRoutes . map SPC


-- | check for duplicate files in [SourceContext]
checkDuplicateFiles :: Bool -> String -> [SourceContext] -> [RouteError]
checkDuplicateFiles doIndexFiles ext = checkDuplicateUsing (makeFileNameFrom doIndexFiles ext)


-- | helper to find duplicates and generate error lists if there are
-- duplicates.  The function takes something from the SourceContext and that is
-- the thing that is checked for duplicates.
checkDuplicateUsing :: (Ord a, Eq a, Show a) => (SourceContext -> a) -> [SourceContext] -> [RouteError]
checkDuplicateUsing f scs = concatMap makeError' $ checkForDuplicates $ fmapToFst f scs


checkForDuplicates :: (Ord a, Eq a) => [(a, b)] -> [[(a, b)]]
checkForDuplicates ps = L.filter ((>1).length) $ L.groupBy ((==) `on` fst) $ L.sortOn fst ps


fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f = fmap (\x -> (f x, x))


makeError' :: Show a => [(a, SourceContext)] -> [RouteError]
makeError' pairs =
    let route = fst $ head pairs
        fileNames = map (fromMaybe "<virtual>" . scRelFilePath . snd) pairs
        spcs = map snd pairs
     in map (go route fileNames) spcs
  where
      go :: Show a => a -> [FilePath] -> SourceContext -> RouteError
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


-- | make an filename from the source page SourceContext.
makeFileNoExtNameFrom :: Bool -> SourceContext -> FilePath
makeFileNoExtNameFrom doIndexPage sc =
    let route = "./" <> routeToFileName (scRoute sc)
        isIndex = scIndexPage sc
     in case (isIndex, doIndexPage) of
         (False, False) -> route
         (False, True)  -> route <> "/index"
         (True, _)      -> route <> "index"


routeToFileName :: String -> FilePath
routeToFileName ""  = ""
routeToFileName "/" = ""
routeToFileName x   = x


makeFileNameFrom :: Bool -> String -> SourceContext -> FilePath
makeFileNameFrom doIndexPage ext sc = makeFileNoExtNameFrom doIndexPage sc <> ext
