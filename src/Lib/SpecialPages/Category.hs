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
-- We create a special VPC for the 'categories' page.  Then the template can
-- call functions to get the categories and the pages in those categories.
-- As an 'extra', we provide another template function 'category_page_route()'
-- that returns the route for that category AND adds in the VPC for that
-- category page (so that it can then be rendered). Note that the template
-- function 'paginate()' will also work, if there are many pages, etc.  It's
-- entirely up to the template author.  The functions here are to generate the
-- VPC's for the category index and category sub-pages (if they are needed).

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

import           Types.SiteGenState     (SiteGenReader (..), SiteGenState (..))

import           Lib.Errors             (SiteGenError (..))
import           Lib.Header             (SourceContext (..),
                                         VirtualPageContext (..))
import qualified Lib.Header             as H
import           Lib.ResolvingTemplates (resolveTemplateName')
import           Lib.RouteUtils         (checkExistingRoute)
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
    mCatPage <- (pure . HashMap.lookup "/categories") =<< PR.asks @SiteGenReader siteRouteMap
    when (isNothing mCatPage) $ do
        let newCatPage = makeVPCForCategories
        addToRenderList [VPC newCatPage]


makeVPCForCategories :: VirtualPageContext
makeVPCForCategories = def { vpcRoute = "/categories"
                           , vpcVimWikiLinkPath = "/categories"
                           , vpcTitle = "Categories"
                           , vpcTemplate = "categories"
                           , vpcIndexPage = False
                           }


getAllCategories
    :: Member (Reader SiteGenReader) r
    => Sem r [String]
getAllCategories = do
    scs <- PR.asks @SiteGenReader siteSourceContexts
    pure $ nub $ sort $ mapMaybe H.spcCategory $ takeSourcePageContents scs


takeSourcePageContents :: [H.SourceContext] -> [H.SourcePageContext]
takeSourcePageContents scs = map (\(H.SPC x) -> x) $ filter isSPC scs
  where
    isSPC (H.SPC _) = True
    isSPC _         = False


pagesForCategory :: String -> [H.SourcePageContext] -> [H.SourcePageContext]
pagesForCategory category = filter hasCategory
  where
      hasCategory s = H.spcCategory s == Just category


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
            $ "generateCategoryPageFor: there is no category: " ++ category
        else do
            let catRoute = "/categories/" <> category
            -- see if there is a user category page set up for the route?
            mCatPage <- (pure . HashMap.lookup catRoute) =<< PR.asks @SiteGenReader siteRouteMap
            when (isJust mCatPage) $ EL.logInfo $ T.pack $ "Category page already exists for: " ++ category
            when (isNothing mCatPage) $ do
                -- see if it's to be rendered or has been rendered?  i.e. does
                -- it exist already?
                exists <- checkExistingRoute catRoute
                when exists $ EL.logInfo $ T.pack $ "Category page has already been added for: " ++ category
                unless exists $ do
                    let newCat = makeVPCForCategory catRoute ("Category: " ++ category)
                    mTname <- resolveTemplateName' (H.vpcTemplate newCat)
                    -- if it does then add the 404 page to the render list
                    when (isNothing mTname) $ EL.logError $ T.pack $ "Not template for category page: " ++ category
                    when (isJust mTname) $ do
                        EL.logInfo $ T.pack $ "Adding category page for: " ++ category
                        addToRenderList [VPC newCat]


makeVPCForCategory :: String -> String -> VirtualPageContext
makeVPCForCategory r t = def { vpcRoute = r
                             , vpcVimWikiLinkPath = r
                             , vpcTitle = t
                             , vpcTemplate = "category"
                             , vpcIndexPage = False
                             }
