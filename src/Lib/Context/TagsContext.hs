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


module Lib.Context.TagsContext where

import           Data.Default             (def)
import qualified Data.List                as L
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Text.Ginger              ((~>))
import qualified Text.Ginger              as TG

import           Colog.Polysemy           (Log)
import qualified Colog.Polysemy           as CP
import           Polysemy                 (Sem)
import qualified Polysemy.Reader          as PR

import           Effect.Ginger            (GingerSemEffects,
                                           GingerSemEffectsToGVal)
import qualified Effect.Logging           as EL

import           Types.Constants
import           Types.Context            (Context, GingerFunctionArgs, RunSem,
                                           RunSemGVal)
import           Types.Errors             (SiteGenError)
import           Types.Header             (SourceMetadata (..))
import           Types.SiteGenState       (SiteGenReader (..))

import           Lib.Context.Core         (contextFromList, tryExtractStringArg)
import qualified Lib.Header               as H
import           Lib.SpecialPages.Tag     (ensureTagPageFor, getAllTags,
                                           pagesForTag)

-- TODO: needed for the instance on TG.ToGVal SourceMetadata; perhaps we should move
-- it?
import           Lib.Context.PageContexts ()
import           Lib.Context.Utils        (filterSourceMetadataItemsUsing,
                                           pagesForFunctionF,
                                           stringArgFuncGValF)


tagsContext
    :: GingerSemEffects r
    => SourceMetadata
    -> Context (RunSem r)
tagsContext sm = do
    let route = smRoute sm
    contextFromList
        $  [("getPagesForTag", pure $ TG.fromFunction pagesForTagF)]
        ++ [("Tags", tagsM)     | smRoute sm == tagsRoute]
        ++ [("Tag", tagM route) | tagsRoutePrefix `L.isPrefixOf` route]


-- | return Just tag if the route forms a tag.
-- /tags/<some-tag> where <some-tag> is a tag that exists
-- If not return Nothing.
tagFromRoute
    :: GingerSemEffects r
    => Route
    -> Sem r (Maybe String)
tagFromRoute route = do
    allTags <- getAllTags
    if tagsRoutePrefix `L.isPrefixOf` route
      then do
          let possible = drop (length tagsRoutePrefix) route
          if possible `elem` allTags
            then pure $ Just possible
            else pure Nothing
      else pure Nothing


-- | get the pages for a tag.
pagesForTagF
    :: GingerSemEffects r
    => TG.Function (RunSem r)
pagesForTagF = stringArgFuncGValF (pagesForFunctionF pagesForTag)


-- | Fetch the tags in the system.  This will provide dictionary called
-- "Tags" that contains a sorted alphanumeric list of Tags, and the
-- then a list of Pages against each category.
tagsM
    :: GingerSemEffects r
    => RunSemGVal r
tagsM = do
    tags <- TG.liftRun getAllTags
    TG.liftRun $ EL.logInfo $ T.pack $ "Tags are: " ++ L.intercalate ", " tags
    pure $ TG.dict [ ("Items", TG.list $ map (\l ->
                            TG.dict ["Name" ~> l,
                                     "Url" ~> (tagsRoutePrefix ++ l)]) tags)
                   , ("pagesFor", TG.fromFunction pagesForTagF)
                   , ("generatePageFor", TG.fromFunction generateTagPageForF)
                   ]


-- | cause the generation of a category page for page name, if it exists.
generateTagPageForF
    :: GingerSemEffects r
    => TG.Function (RunSem r)
generateTagPageForF args = do
    TG.liftRun $ EL.logDebug "generateTagPageFor called"
    stringArgFuncGValF f args
  where
      f tag = TG.liftRun (ensureTagPageFor tag) >> pure def


-- | Provide variables and functions for a Tag page
-- in the /tags/<some-tag> space.
-- At the moment, it provides the name if the route does actually end up on a
-- category.
tagM
    :: GingerSemEffects r
    => Route
    -> RunSemGVal r
tagM route = do
    mTag <- TG.liftRun $ tagFromRoute route
    pages <- case mTag of
        Just tag ->
            TG.liftRun $ filterSourceMetadataItemsUsing pagesForTag tag
        Nothing -> pure []
    pure $ TG.dict [ "Name" ~> mTag
                   , "Pages" ~> pages
                   ]
