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

module Lib.SpecialPages.Tag where

-- | Tag Page handling
--
-- The tags allow a page to be 'in' several tags.
-- We create an ordinary VPC for the 'categories' page.  Then the template can
-- call functions to get the tags and the pages in those tags.


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

import           Lib.ResolvingTemplates (resolveTemplateName)
import           Lib.RouteUtils         (checkExistingRoute, routeToConcreteSc)
import           Lib.SiteGenConfig      (ConfigException, SiteGenConfig (..))
import           Lib.SiteGenState       (addToRenderList)


resolveTagsPage
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
resolveTagsPage = do
    mTagsPage <- (pure . HashMap.lookup tagsRoute) =<< PR.asks @SiteGenReader siteRouteMap
    if isNothing mTagsPage
      then do
        EL.logInfo "Using Internal Tags page"
        let newTagsPage = makeVSMForTags
        addToRenderList [newTagsPage]
      else EL.logInfo "Using user supplied tags page"


makeVSMForTags :: SourceMetadata
makeVSMForTags = def { smRoute = tagsRoute
                     , smVimWikiLinkPath = tagsRoute
                     , smTitle = "Tags"
                     , smTemplate = "tags"
                     , smIndexPage = False
                     }


getAllTags
    :: Member (Reader SiteGenReader) r
    => Sem r [String]
getAllTags = do
    sms <- PR.asks @SiteGenReader siteSourceMetadataItems
    pure $ nub $ sort $ concatMap smTags sms


pagesForTag :: String -> [SourceMetadata] -> [SourceMetadata]
pagesForTag tag = filter hasTag
  where
      hasTag s = tag `elem` smTags s


ensureTagPageFor
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
ensureTagPageFor tag = do
    EL.logDebug $ T.pack $ "ensureTagPageFor: " ++ tag
    tags <- getAllTags
    if tag `notElem` tags
        then EL.logError
            $ T.pack
            $ "ensureTagPageFor: there is no tag: " ++ tag
        else do
            let tagRoute = tagsRoutePrefix <> tag
            -- see if there is a user category page set up for the route?
            mTagPage <- routeToConcreteSc tagRoute
            case mTagPage of
                Just _ -> EL.logInfo $ T.pack $ "Tag page already exists for: " ++ tag
                Nothing -> do
                    -- see if it's to be rendered or has been rendered?  i.e. does
                    -- it exist already?
                    exists <- checkExistingRoute tagRoute
                    if exists
                      then EL.logInfo $ T.pack $ "Tag page has already been added for: " ++ tag
                      else do
                        let newTag = makeVSMForTag tagRoute ("Tag: " ++ tag)
                        mTname <- resolveTemplateName (smTemplate newTag)
                        -- if it does then add the tag page to the render list
                        case mTname of
                            Nothing -> EL.logError $ T.pack $ "No template for tag page: " ++ tag
                            Just _ ->  do
                                EL.logInfo $ T.pack $ "Adding tag page for: " ++ tag
                                addToRenderList [newTag]


makeVSMForTag :: String -> String -> SourceMetadata
makeVSMForTag r t = def { smRoute = r
                        , smVimWikiLinkPath = r
                        , smTitle = t
                        , smTemplate = "tag"
                        , smIndexPage = False
                        }
