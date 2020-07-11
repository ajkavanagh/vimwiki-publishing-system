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

import           Effect.Ginger             (GingerSemEffects)
import qualified Effect.Logging            as EL

import           Lib.Context.Core          (contextFromList,
                                            tryExtractStringArg)
import           Lib.Errors                (SiteGenError)
import qualified Lib.Header                as H
import           Lib.SpecialPages.Category (ensureCategoryPageFor,
                                            getAllCategories, pagesForCategory,
                                            takeSourcePageContents)

import           Types.Context             (Context, RunSem, RunSemGVal)
import           Types.SiteGenState        (Route, SiteGenReader (..))

-- TODO: needed for the instance on TG.ToGVal SourceContext; perhaps we should move
-- that?
import           Lib.Context.PageContexts  ()


categoriesContext
    :: GingerSemEffects r
    => H.SourceContext
    -> Context (RunSem r)
categoriesContext sc = do
    let route = H.scRoute sc
    contextFromList
        $  [("getPagesForCategory", pure $ TG.fromFunction pagesForCategoryF)]
        ++ [("Categories", categoriesM)   | H.scRoute sc == "/categories"]
        ++ [("Category", categoryM route) | "/categories/" `L.isPrefixOf` route]


-- | return Just category if the route forms a category.
-- /categories/<some-cat> where <some-cat> is a category that exists
-- If not return Nothing.
categoryFromRoute
    :: GingerSemEffects r
    => Route
    -> Sem r (Maybe String)
categoryFromRoute route = do
    allCats <- getAllCategories
    if "/categories/" `L.isPrefixOf` route
      then do
          let possible = drop (length "/categories/") route
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
    TG.liftRun $ EL.logInfo $ T.pack $ "Categories are: " ++ L.intercalate ", " categories
    pure $ TG.dict [ ("Labels", TG.list $ map (\l -> TG.dict ["Name" ~> l, "Url" ~> ("/categories/" ++ l)]) categories)
                   , ("pagesFor", TG.fromFunction pagesForCategoryF)
                   , ("generatePageFor", TG.fromFunction generateCategoryPageForF)
                   ]


-- | get the pages for a category.
pagesForCategoryF
    :: GingerSemEffects r
    => TG.Function (RunSem r)
pagesForCategoryF args = do
    TG.liftRun $ EL.logDebug "category pagesForF called"
    case tryExtractStringArg args of
        Nothing -> do
            TG.liftRun $ EL.logError "No text arg passed to pagesFor!"
            pure def
        Just category -> do
            scs <- TG.liftRun $ PR.asks @SiteGenReader siteSourceContexts
            let spcs = pagesForCategory (T.unpack category) $ takeSourcePageContents scs
            TG.liftRun $ EL.logDebug $ T.pack $ "pagesForF(" ++ T.unpack category ++") =" ++ L.intercalate ", " (map H.spcRoute spcs)
            pure $ TG.list $ map TG.toGVal spcs


-- | cause the generation of a category page for page name, if it exists.
generateCategoryPageForF
    :: GingerSemEffects r
    => TG.Function (RunSem r)
generateCategoryPageForF args = do
    TG.liftRun $ EL.logDebug "generateCategoryPageFor called"
    case tryExtractStringArg args of
        Nothing -> do
            TG.liftRun $ EL.logError "No text arg passed to generateCategoryPageFor!"
            pure def
        Just category -> do
            TG.liftRun $ ensureCategoryPageFor (T.unpack category)
            pure def

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
        Just category -> do
            scs <- TG.liftRun $ PR.asks @SiteGenReader siteSourceContexts
            let spcs = pagesForCategory category $ takeSourcePageContents scs
            pure spcs
        Nothing -> pure []
    pure $ TG.dict [ "Name" ~> mCategory
                   , "Pages" ~> pages
                   ]
