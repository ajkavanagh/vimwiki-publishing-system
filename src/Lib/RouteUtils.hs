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
    ( RouteError(..)
    , addVSMIndexPages
    , checkDuplicateRoutes
    , checkExistingRoute
    , ensureIndexRoutesIn
    , findMissingIndexRoutes
    , indexRoutesFor
    , isIndexRoute
    , makeFileNameFrom
    , makeFileNoExtNameFrom
    , routeToConcreteSc
    , sameLevelRoutesAs
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

import           Types.Constants
import           Types.SiteGenState  (SiteGenReader (..), SiteGenState (..))

import           Types.Errors        (SiteGenError (..))
import           Types.Header        (SourceMetadata (..))
import           Types.RouteUtils

import           Lib.Header          (resolveLinkFor)


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
    page <- routeToConcreteSc route
    falseOr page $ do
        page' <- (pure . HashMap.lookup route) =<< PS.gets @SiteGenState sitePagesRendered
        falseOr page' $ do
            srl <- PS.gets @SiteGenState siteRenderList
            pure $ elem route $ map smRoute srl


-- Helper that takes a Maybe value (which it doesn't care about), and if Nothing
-- returns m False, but if Just _ then returns the evaluation of the passed
-- function.
falseOr :: Monad m => Maybe a -> m Bool -> m Bool
falseOr v f = maybe (pure False) (const f) v


-- | look up a route to a concrete (actual page) rather than a dynamically
-- generated page from, say, categories, etc.
routeToConcreteSc
    :: ( Member (Reader SiteGenReader) r
       )
    => Route
    -> Sem r (Maybe SourceMetadata)
routeToConcreteSc r = (pure . HashMap.lookup r) =<< PR.asks @SiteGenReader siteRouteMap



-- | checks for duplicate routes in the SPCs, then generate any missing index
-- pages and finally check for inconsistent indexes (i.e. where an output
-- filename can't be generated due to index pages.
validateRoutesInSMs
    :: Bool                                -- whether we are generating "indexPages"
    -> String                              -- The final extension ".html"
    -> [SourceMetadata]
    -> Either [RouteError] [SourceMetadata]
validateRoutesInSMs doIndexFileNames ext sms =
    let errors = checkDuplicateRoutes sms
     in if not (null errors)
          then Left errors
          else let sms' = ensureIndexRoutesIn sms
                   scs = addVSMIndexPages sms'
                   ferrors = checkDuplicateFiles doIndexFileNames ext scs
                in if not (null ferrors)
                     then Left errors
                     else Right scs



-- | check for duplicate routes in [SourceMetadata]
checkDuplicateRoutes :: [SourceMetadata] -> [RouteError]
checkDuplicateRoutes = checkDuplicateUsing resolveLinkFor


-- | check for duplicate files in [SourceMetadata]
checkDuplicateFiles :: Bool -> String -> [SourceMetadata] -> [RouteError]
checkDuplicateFiles doIndexFiles ext = checkDuplicateUsing (makeFileNameFrom doIndexFiles ext)


-- | helper to find duplicates and generate error lists if there are
-- duplicates.  The function takes something from the SourceMetadata and that is
-- the thing that is checked for duplicates.
checkDuplicateUsing :: (Ord a, Eq a, Show a)
                    => (SourceMetadata -> a)
                    -> [SourceMetadata]
                    -> [RouteError]
checkDuplicateUsing f sms = concatMap makeError' $ checkForDuplicates $ fmapToFst f sms


checkForDuplicates :: (Ord a, Eq a) => [(a, b)] -> [[(a, b)]]
checkForDuplicates ps = L.filter ((>1).length) $ L.groupBy ((==) `on` fst) $ L.sortOn fst ps


fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f = fmap (\x -> (f x, x))


fmapOnFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
fmapOnFst f = fmap (first f)


makeError' :: Show a => [(a, SourceMetadata)] -> [RouteError]
makeError' pairs =
    let route = fst $ head pairs
        fileNames = map (fromMaybe "<virtual>" . smRelFilePath . snd) pairs
        spcs = map snd pairs
     in map (go route fileNames) spcs
  where
      go :: Show a => a -> [FilePath] -> SourceMetadata -> RouteError
      go r fps sm = DuplicateRouteError sm (T.pack ("Pages share same route: "
                                           <> show r
                                           <> ", filenames: "
                                           <> L.intercalate ", " fps))


-- | fix the index page routes in SourceMetadata records that are an index
-- page.
ensureIndexRoutesIn :: [SourceMetadata] -> [SourceMetadata]
ensureIndexRoutesIn sms = go <$> sms
  where go sm = let r = smRoute sm
                 in if smIndexPage sm && not (isIndexRoute r)
                      then sm { smRoute=r <> "/" }
                      else sm


-- | identify the missing index pages in the routes.
-- We are looking for routes that have a 'directories' in them for which no
-- index page exists at that point.
-- e.g. thing/one route needs a thing/ page.
findMissingIndexRoutes :: [SourceMetadata] -> [String]
findMissingIndexRoutes sms =
    let allRoutes = L.nub $ L.sort $ concatMap (indexRoutesFor . smRoute) sms
        actualRoutes = smRoute <$> filter smIndexPage sms
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


-- | ensure that we have a set of Virtual SourceMetadata records for each missing
-- index page.
addVSMIndexPages :: [SourceMetadata] -> [SourceMetadata]
addVSMIndexPages sms =
    let missingIndexes = findMissingIndexRoutes sms
        vsms = map makeVSMForIndex missingIndexes
     in sms ++ vsms


makeVSMForIndex :: String -> SourceMetadata
makeVSMForIndex route = def { smRoute = route
                            , smVimWikiLinkPath = route
                            , smTitle = "Page: " ++ route
                            , smTemplate = "index"
                            , smIndexPage = True
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

-- | make an filename from the source page SourceMetadata.
-- If the permalink is defined, then that is used, otherwise it is derived from
-- the route.
makeFileNoExtNameFrom :: Bool -> SourceMetadata -> FilePath
makeFileNoExtNameFrom doIndexPage sm =
    let route = "." <> routeToFileName (resolveLinkFor sm)
        isIndex = smIndexPage sm
     in case (isIndex, doIndexPage) of
         (False, False) -> route
         (False, True)  -> route <> "/index"
         (True, _)      -> route <> "index"


routeToFileName :: String -> FilePath
routeToFileName ""  = "/"
routeToFileName "/" = "/"
routeToFileName x   = x


-- resolve the filepath for the SourceMetadata
-- There is a set of special cases (the first is the atom.feed - which just
-- returns the atom.feed name; we'll make it generic whe the 2nd one is needed
makeFileNameFrom :: Bool -> String -> SourceMetadata -> FilePath
makeFileNameFrom doIndexPage ext sm =
    if smRoute sm == atomFeedRoute
      then "feed.atom"
      else makeFileNoExtNameFrom doIndexPage sm <> ext


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
