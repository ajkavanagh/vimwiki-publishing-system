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

import           Effect.Ginger     (GingerSemEffects)

import           Lib.Context.Core  (contextFromList)
import qualified Lib.SiteGenConfig as S
import           Types.Context     (Context, RunSem, RunSemGVal)

-- provide the @Context m@ for the SiteGenConfig record
-- This assumes that a Sem r Reader for SiteGenConfig exists
-- so that it can be used to generate the GVal m


siteGenConfigContext :: GingerSemEffects r => Context (RunSem r)
siteGenConfigContext = contextFromList [("Site", siteGenMGValM)]


siteGenMGValM :: GingerSemEffects r => RunSemGVal r
siteGenMGValM = do
    sgc <- TG.liftRun $ PR.ask @S.SiteGenConfig
    pure $ TG.dict
        [ "siteYaml"           ~> S.sgcSiteYaml sgc
        , "siteId"             ~> S.sgcSiteId sgc
        , "source"             ~> S.sgcSource sgc
        , "outputDir"          ~> S.sgcOutputDir sgc
        , "extension"          ~> S.sgcExtension sgc
        , "indexPageName"      ~> S.sgcIndexPageName sgc
        , "templatesDir"       ~> S.sgcTemplatesDir sgc
        , "templateExt"        ~> S.sgcTemplateExt sgc
        , "staticDir"          ~> S.sgcStaticDir sgc
        , "generateTags"       ~> S.sgcGenerateTags sgc
        , "generateCategories" ~> S.sgcGenerateCategories sgc
        , "publishDrafts"      ~> S.sgcPublishDrafts sgc
        , "indexFiles"         ~> S.sgcIndexFiles sgc
        , "maxSummaryWords"    ~> S.sgcMaxSummaryWords sgc
        , "Params"             ~> S.sgcParams sgc
        ]
