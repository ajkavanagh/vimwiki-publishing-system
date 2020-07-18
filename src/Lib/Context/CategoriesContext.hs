{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}

-- needed for the instance ToGVal m a instances that use the RunSem r monad
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.CategoriesContext where

import           Data.Default              (def)
import qualified Data.List                 as L
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Text.Ginger               ((~>))
import qualified Text.Ginger               as TG

import           Colog.Polysemy            (Log)
import qualified Colog.Polysemy            as CP
import           Polysemy                  (Sem)
import qualified Polysemy.Reader           as PR

import           Effect.Ginger             (GingerSemEffects,
                                            GingerSemEffectsToGVal)
import qualified Effect.Logging            as EL

import           Types.Constants
import           Types.Context             (Context, GingerFunctionArgs, RunSem,
                                            RunSemGVal)
import           Types.Errors              (SiteGenError)
import           Types.Header              (SourceMetadata (..))
import           Types.SiteGenState        (SiteGenReader (..))

import           Lib.Context.Core          (contextFromList,
                                            tryExtractStringArg)
import           Lib.SpecialPages.Category (ensureCategoryPageFor,
                                            getAllCategories, pagesForCategory)

-- TODO: needed for the instance on TG.ToGVal SourceMetadata; perhaps we should move
-- it?
import           Lib.Context.PageContexts  ()
import           Lib.Context.Utils         (filterSourceMetadataItemsUsing,
                                            pagesForFunctionF,
                                            stringArgFuncGValF)


categoriesContext
    :: GingerSemEffects r
    => SourceMetadata
    -> Context (RunSem r)
categoriesContext sm = do
    let route = smRoute sm
    contextFromList
        $  [("getPagesForCategory", pure $ TG.fromFunction pagesForCategoryF)]
        ++ [("Categories", categoriesM)   | smRoute sm == categoriesRoute]
        ++ [("Category", categoryM route) | categoriesRoutePrefix `L.isPrefixOf` route]


-- | return Just category if the route forms a category.
-- /categories/<some-cat> where <some-cat> is a category that exists
-- If not return Nothing.
categoryFromRoute
    :: GingerSemEffects r
    => Route
    -> Sem r (Maybe String)
categoryFromRoute route = do
    allCats <- getAllCategories
    if categoriesRoutePrefix `L.isPrefixOf` route
      then do
          let possible = drop (length categoriesRoutePrefix) route
          if possible `elem` allCats
            then pure $ Just possible
            else pure Nothing
      else pure Nothing


-- | Fetch the categories in the system.  This will provide dictionary called
-- "Categories" that contains a sorted alphanumeric list of categories, and the
-- then a list of Pages against each category.
categoriesM
    :: GingerSemEffects r
    => RunSemGVal r
categoriesM = do
    categories <- TG.liftRun getAllCategories
    pure $ TG.dict [ ("Items", TG.list $ map (\l ->
                            TG.dict ["Name" ~> l,
                                     "Url" ~> (categoriesRoutePrefix ++ l)]) categories)
                   , ("pagesFor", TG.fromFunction pagesForCategoryF)
                   , ("generatePageFor", TG.fromFunction generateCategoryPageForF)
                   ]


-- | get the pages for a category.
pagesForCategoryF
    :: GingerSemEffects r
    => TG.Function (RunSem r)
pagesForCategoryF = stringArgFuncGValF (pagesForFunctionF pagesForCategory)


-- | cause the generation of a category page for page name, if it exists.
generateCategoryPageForF
    :: GingerSemEffects r
    => TG.Function (RunSem r)
generateCategoryPageForF args = do
    TG.liftRun $ EL.logDebug "generateCategoryPageFor called"
    stringArgFuncGValF f args
  where
      f category = TG.liftRun (ensureCategoryPageFor category) >> pure def


-- | Provide variables and functions for a Category page
-- in the /categories/<some-cat> space.
-- At the moment, it provides the name if the route does actually end up on a
-- category.
categoryM
    :: GingerSemEffects r
    => Route
    -> RunSemGVal r
categoryM route = do
    mCategory <- TG.liftRun $ categoryFromRoute route
    pages <- case mCategory of
        Just category ->
            TG.liftRun $ filterSourceMetadataItemsUsing pagesForCategory category
        Nothing -> pure []
    pure $ TG.dict [ "Name" ~> mCategory
                   , "Pages" ~> pages
                   ]
