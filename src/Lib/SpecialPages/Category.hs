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

module Lib.SpecialPages.Category where

-- | Category Page handling
--
-- The categories allow a page to be 'in' a category.
-- We create an ordinary VPC for the 'categories' page.  Then the template can
-- call functions to get the categories and the pages in those categories.


import           Control.Monad          (unless, when)

import           Data.Default           (def)
import qualified Data.HashMap.Strict    as HashMap
import           Data.List              (nub, sort)
import           Data.Maybe             (isJust, isNothing, mapMaybe)
import qualified Data.Text              as T

import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP

import           Effect.File            (File, FileException)
import           Effect.Logging         (LoggingMessage)
import qualified Effect.Logging         as EL

import           Types.Constants
import           Types.Errors           (SiteGenError (..))
import           Types.Header           (SourceMetadata (..))
import           Types.SiteGenState     (SiteGenReader (..), SiteGenState (..))

import           Lib.ResolvingTemplates (resolveTemplateName')
import           Lib.RouteUtils         (checkExistingRoute, routeToConcreteSc)
import           Lib.SiteGenConfig      (ConfigException, SiteGenConfig (..))
import           Lib.SiteGenState       (addToRenderList)



resolveCategoriesPage
    :: ( Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (State SiteGenState) r
       , Member File r
       , Member (Error FileException) r
       , Member (Error SiteGenError) r
       , Member (Error ConfigException) r
       , Member (Log LoggingMessage) r
       )
    =>  Sem r ()
resolveCategoriesPage = do
    mCatPage <- routeToConcreteSc categoriesRoute
    if isNothing mCatPage
      then do
        EL.logInfo "Using Internal Categories page"
        let newCatPage = makeVSMForCategories
        addToRenderList [newCatPage]
      else EL.logInfo "Using user supplied categories page"


makeVSMForCategories :: SourceMetadata
makeVSMForCategories = def { smRoute = categoriesRoute
                           , smVimWikiLinkPath = categoriesRoute
                           , smTitle = "Categories"
                           , smTemplate = "categories"
                           , smIndexPage = False
                           }


getAllCategories
    :: Member (Reader SiteGenReader) r
    => Sem r [String]
getAllCategories = do
    sms <- PR.asks @SiteGenReader siteSourceMetadataItems
    pure $ nub $ sort $ concatMap smCategories sms


pagesForCategory :: String -> [SourceMetadata] -> [SourceMetadata]
pagesForCategory category = filter hasCategory
  where
      hasCategory s = category `elem` smCategories s


ensureCategoryPageFor
    :: ( Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (State SiteGenState) r
       , Member File r
       , Member (Error FileException) r
       , Member (Error SiteGenError) r
       , Member (Error ConfigException) r
       , Member (Log LoggingMessage) r
       )
    => String
    -> Sem r ()
ensureCategoryPageFor category = do
    EL.logDebug $ T.pack $ "ensureCategoryPageFor: " ++ category
    categories <- getAllCategories
    if category `notElem` categories
        then EL.logError
            $ T.pack
            $ "ensureCategoryPageFor: there is no category: " ++ category
        else do
            let catRoute = categoriesRoutePrefix <> category
            -- see if there is a user category page set up for the route?
            mCatPage <- routeToConcreteSc catRoute
            case mCatPage of
                Just _ -> EL.logInfo $ T.pack $ "Category page already exists for: " ++ category
                Nothing -> do
                    -- see if it's to be rendered or has been rendered?  i.e. does
                    -- it exist already?
                    exists <- checkExistingRoute catRoute
                    if exists
                      then EL.logInfo $ T.pack $ "Category page has already been added for: " ++ category
                      else do
                        let newCat = makeVSMForCategory catRoute ("Category: " ++ category)
                        mTname <- resolveTemplateName' (smTemplate newCat)
                        -- if it does then add the category page to the render list
                        case mTname of
                            Nothing -> EL.logError $ T.pack $ "No template for category page: " ++ category
                            Just _ ->  do
                                EL.logInfo $ T.pack $ "Adding category page for: " ++ category
                                addToRenderList [newCat]


makeVSMForCategory :: String -> String -> SourceMetadata
makeVSMForCategory r t = def { smRoute = r
                             , smVimWikiLinkPath = r
                             , smTitle = t
                             , smTemplate = "category"
                             , smIndexPage = False
                             }
