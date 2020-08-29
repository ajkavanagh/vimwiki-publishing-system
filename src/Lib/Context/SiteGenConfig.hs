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

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.SiteGenConfig where


import           Text.Ginger       ((~>))
import qualified Text.Ginger       as TG

import           Colog.Polysemy    (Log)
import qualified Colog.Polysemy    as CP
import           Polysemy          (Member)
import           Polysemy.Reader   (Reader)
import qualified Polysemy.Reader   as PR

import           Types.Constants
import           Types.Context     (Context, RunSem, RunSemGVal)

import           Effect.Ginger     (GingerSemEffects)

import           Lib.Context.Core  (contextFromList)
import qualified Lib.SiteGenConfig as S

-- provide the @Context m@ for the SiteGenConfig record
-- This assumes that a Sem r Reader for SiteGenConfig exists
-- so that it can be used to generate the GVal m


siteGenConfigContext :: GingerSemEffects r => Context (RunSem r)
siteGenConfigContext = contextFromList [("Site", siteGenMGValM)]


siteGenMGValM :: GingerSemEffects r => RunSemGVal r
siteGenMGValM = do
    sgc <- TG.liftRun $ PR.ask @S.SiteGenConfig
    pure $ TG.dict $
        [ "SiteYaml"           ~> S.sgcSiteYaml sgc
        , "SiteId"             ~> S.sgcSiteId sgc
        , "SiteURL"            ~> (show <$> S.sgcSiteUrl sgc)
        , "Source"             ~> S.sgcSource sgc
        , "OutputDir"          ~> S.sgcOutputDir sgc
        , "Extension"          ~> S.sgcExtension sgc
        , "IndexPageName"      ~> S.sgcIndexPageName sgc
        , "ThemeDir"           ~> S.sgcThemeDir sgc
        , "TemplatesDirs"      ~> S.sgcTemplatesDirs sgc
        , "TemplateExt"        ~> S.sgcTemplateExt sgc
        , "StaticDirs"         ~> S.sgcStaticDirs sgc
        , "GenerateTags"       ~> S.sgcGenerateTags sgc
        , "GenerateCategories" ~> S.sgcGenerateCategories sgc
        , "PublishDrafts"      ~> S.sgcPublishDrafts sgc
        , "IndexFiles"         ~> S.sgcIndexFiles sgc
        , "MaxSummaryWords"    ~> S.sgcMaxSummaryWords sgc
        , "SkylightStyle"      ~> S.sgcSkylightStyle sgc
        , "ExtraDebug"         ~> S.sgcExtraDebug sgc
        , "Params"             ~> S.sgcParams sgc
        ]
        ++ [ "RSSLink" ~> atomFeedRoute  | S.sgcGenerateFeed sgc ]
