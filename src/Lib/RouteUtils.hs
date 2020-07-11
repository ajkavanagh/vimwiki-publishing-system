{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


module Lib.RouteUtils
    ( checkDuplicateRoutes
    , checkDuplicateRoutesSPC
    , ensureIndexRoutesIn
    , indexRoutesFor
    , isIndexRoute
    , findMissingIndexRoutesSPC
    , addVPCIndexPages
    , makeFileNameFrom
    , makeFileNoExtNameFrom
    , sameLevelRoutesAs
    , checkExistingRoute
    , RouteError(..)
    ) where

import           Data.Bifunctor      (first)
import           Data.Default.Class  (Default, def)
import           Data.Function       (on)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as L
import qualified Data.List.Split     as LS
import           Data.Maybe          (fromMaybe, isNothing)
import           Data.Ord            (comparing)
import qualified Data.Text           as T

import qualified System.FilePath     as F

import           Polysemy            (Member, Sem)
import           Polysemy.Error      (Error)
import           Polysemy.Reader     (Reader)
import qualified Polysemy.Reader     as PR
import           Polysemy.State      (State)
import qualified Polysemy.State      as PS

import           Colog.Polysemy      (Log)
import qualified Colog.Polysemy      as CP

import           Types.SiteGenState  (Route, SiteGenReader (..),
                                      SiteGenState (..))

import           Lib.Errors          (SiteGenError (..))
import           Lib.Header          (SourceContext (..),
                                      SourcePageContext (..),
                                      VirtualPageContext (..), scIndexPage,
                                      scRelFilePath, scRoute)


data RouteError = DuplicateRouteError SourceContext T.Text
                  deriving (Eq, Show)



-- see if a route exists in the list of pages to render, or the pages that have
-- been rendered.  This is so that functions can avoid dynamically generating
-- duplicate routes.
checkExistingRoute
    :: ( Member (Reader SiteGenReader) r
       , Member (State SiteGenState) r
       )
    => Route
    -> Sem r Bool
checkExistingRoute route = do
    -- check if the route is in the route map (this is quick)
    page <- (pure . HashMap.lookup route) =<< PR.asks @SiteGenReader siteRouteMap
    falseOr page $ do
        page' <- (pure . HashMap.lookup route) =<< PS.gets @SiteGenState sitePagesRendered
        falseOr page' $ do
            srl <- PS.gets @SiteGenState siteRenderList
            pure $ elem route $ map scRoute srl


falseOr :: Monad m => Maybe a -> m Bool -> m Bool
falseOr v f =
    if isNothing v
      then pure False
      else f


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


fmapOnFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
fmapOnFst f = fmap (first f)


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
                  in if spcIndexPage spc && not (isIndexRoute r)
                       then spc { spcRoute=r <> "/" }
                       else spc


-- | identify the missing index pages in the routes.
-- We are looking for routes that have a 'directories' in them for which no
-- index page exists at that point.
-- e.g. thing/one route needs a thing/ page.
findMissingIndexRoutesSPC :: [SourcePageContext] -> [String]
findMissingIndexRoutesSPC spcs =
    let allRoutes = L.nub $ L.sort $ concatMap (indexRoutesFor . spcRoute) spcs
        actualRoutes = spcRoute <$> filter spcIndexPage spcs
     in allRoutes L.\\ actualRoutes


isIndexRoute :: String -> Bool
isIndexRoute "" = False
isIndexRoute s  = last s == '/'


-- | compute the index routes for a route.  i.e. if we have some/thing then we
-- need to generate the index routes "some/" and "/"
indexRoutesFor :: String -> [String]
indexRoutesFor s =
    let parts = filter (not . null) $ LS.splitOn "/" s
     in case parts of
         [] -> ["/"]
         xs -> "/" : map (\p -> "/" <> p <> "/") (L.init parts)


-- | ensure that we have a set of VirtualPageContext records for each missing
-- index page.  Note that SourceContext is a wrapper around the
-- SourcePageContext and the VirtualPageContext, and the SourceClass is used to
-- paper over the cracks in most places.
addVPCIndexPages :: [SourcePageContext] -> [SourceContext]
addVPCIndexPages spcs =
    let missingIndexes = findMissingIndexRoutesSPC spcs
        vpcs = map makeVPCForIndex missingIndexes
     in map SPC spcs ++ map VPC vpcs


makeVPCForIndex :: String -> VirtualPageContext
makeVPCForIndex route = def { vpcRoute = route
                            , vpcVimWikiLinkPath = route
                            , vpcTitle = "Page: " ++ route
                            , vpcTemplate = "index"
                            , vpcIndexPage = True
                            }


{-
    Work out where files go.

    Firstly: No index-files

    Route:                File Name
    ------                ---------
    /                     index.html
    /hello                hello.html
    /hello/               hello/index.html
    /hello/there          hello/there.html

    Secondly: With index-files

    Route:                File Name
    ------                ---------
    /                     index.html
    /hello                hello/index.html
    /hello/               hello/index.html  -- clash!
    /hello/there          hello/there/index.html

    So duplicate routes have to check for two routes where "thing" and "thing/"
    both exist as otherwise there will be an error!
-}

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


-- | work out routes at the same level.  i.e. for the paging functions.
-- If the route is an index (i.e. ends in /) then search the routes at the level
-- down, otherwise it's the routes at the same level.
-- We work with a function that does an a->String, so that we can work with
-- string (pass id) and also other objects that contain a route.
-- Note DOES return the route if it's not an index at the same level.
sameLevelRoutesAs :: (a -> String) -> String -> [a] -> [a]
sameLevelRoutesAs _ "" _ = error "Don't pass empty String to sameLevelRoutesAs!"
sameLevelRoutesAs f route as =
    let route' = if last route == '/'
                   then route
                   else L.intercalate "/" (init $ LS.splitOn "/" route) <> "/"
        lenRoute = length (LS.splitOn "/" route')
        routePairs = fmapToFst f as
        possiblePairs = filter ((route' `L.isPrefixOf`).fst) routePairs
        lenRoutes = fmapOnFst (length. LS.splitOn "/") possiblePairs
        matched = filter ((==lenRoute).fst) lenRoutes
     in map snd matched
