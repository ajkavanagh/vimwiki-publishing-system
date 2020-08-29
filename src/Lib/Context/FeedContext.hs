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


module Lib.Context.FeedContext where

import           Control.Applicative         ((<|>))

import           Data.Time.LocalTime         (LocalTime, utc, utcToLocalTime)
import           Text.Ginger                 ((~>))
import qualified Text.Ginger                 as TG

import           Polysemy                    (Member, Sem)
import           Polysemy.Reader             (Reader)
import qualified Polysemy.Reader             as PR

import           Types.Constants
import           Types.Context               (Context, ContextObject (..),
                                              ContextObjectTypes (..), RunSem,
                                              RunSemGVal,
                                              gValContextObjectTypeDictItemFor)
import           Types.Header                (SourceMetadata (..))

import           Effect.Ginger               (GingerSemEffects,
                                              GingerSemEffectsToGVal)
import qualified Effect.Time                 as ET

import           Lib.Context.Core            (contextFromList, emptyContext,
                                              tryExtractStringArg)
import           Lib.Context.DynamicContexts (contentDynamic, summaryDynamic)
import           Lib.Header                  (isContentFile, resolveLinkFor)
import           Lib.SiteGenState            (SiteGenReader (..),
                                              SiteGenState (..),
                                              addToRenderList)


data FeedItem = FeedItem
    { fiIncludeSummary :: !Bool
    , fiSM             :: !SourceMetadata
    }



feedContext
    :: GingerSemEffects r
    => SourceMetadata
    -> Context (RunSem r)
feedContext sm = do
    let route = smRoute sm
    if smRoute sm == atomFeedRoute
      then contextFromList [("Feed", feedM)]
      else emptyContext


feedM
    :: GingerSemEffects r
    => RunSemGVal r
feedM = do
    items <- TG.liftRun getFeedItems
    time <- TG.liftRun ET.getCurrentTime
    pure $ TG.dict
        [ ("Items",    TG.list $ map TG.toGVal items)
        , "Updated" ~> utcToLocalTime utc time
        ]

-- | get the feed items into a new structure; this is purely so that we can have
-- a ToGVal instance for them.
getFeedItems
    :: Member (Reader SiteGenReader) r
    => Sem r [FeedItem]
getFeedItems = do
    sms <- PR.asks @SiteGenReader siteSourceMetadataItems
    pure $ map (makeFeedItem False) $ filter isContentFile sms


makeFeedItem :: Bool -> SourceMetadata -> FeedItem
makeFeedItem includeContent sm = FeedItem { fiIncludeSummary=includeContent
                                          , fiSM=sm
                                          }


instance GingerSemEffects r => TG.ToGVal (RunSem r) FeedItem where
    toGVal fi = let sm = fiSM fi in
        TG.dict
            [ gValContextObjectTypeDictItemFor SMObjectType
            , "Permalink"       ~> resolveLinkFor sm  -- the permalink is an override over the route
            , "Title"           ~> smTitle sm
            , "Template"        ~> smTemplate sm
            , "Tags"            ~> smTags sm
            , "Updated"          ~> (utcToLocalTime utc <$> (smUpdated sm <|> smDate sm))
            , "Authors"         ~> smAuthors sm
            , "IncludeContent"  ~> fiIncludeSummary fi
            -- note these are lower case initial as they are functions and need to
            -- be called from the template
            , ("content",       TG.fromFunction (contentDynamic sm))
            , ("summary",       TG.fromFunction (summaryDynamic sm))
            ]
