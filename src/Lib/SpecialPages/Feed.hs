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

module Lib.SpecialPages.Feed where

-- | Feed page handling is fun.
--
-- Generate the feed page so that it gets rendered

import           Control.Monad          (when)

import           Data.Default           (def)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Maybe             (isJust, isNothing)

import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP

import           Effect.File            (File, FileException)
import qualified Effect.File            as EF
import           Effect.Logging         (LoggingMessage)
import qualified Effect.Logging         as EL

import           Types.Constants
import           Types.Errors           (SiteGenError (..))
import           Types.Header           (SourceMetadata (..))
import           Types.SiteGenState     (SiteGenReader (..), SiteGenState (..))

import           Lib.ResolvingTemplates (resolveTemplateName)
import           Lib.SiteGenConfig      (ConfigException, SiteGenConfig (..))
import           Lib.SiteGenState       (addToRenderList)




resolveFeedPage
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
resolveFeedPage = do
    mFeedSM <- (pure . HashMap.lookup atomFeedRoute) =<< PR.asks @SiteGenReader siteRouteMap
    -- if it already exists, then no need to do more, otherwise:
    when (isNothing mFeedSM) $ do
        let newFeedSM = makeVSMForFeed
        EL.logInfo "Adding default Feed handler."
        addToRenderList [newFeedSM]


makeVSMForFeed :: SourceMetadata
makeVSMForFeed = def
    { smRoute = atomFeedRoute
    , smVimWikiLinkPath = atomFeedRoute
    , smTitle = "Atom Feed"
    , smTemplate = "feed.atom"
    , smIndexPage = False
    }

