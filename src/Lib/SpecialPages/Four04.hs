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

module Lib.SpecialPages.Four04 where

-- | 404 page handling is fun.
--
-- 1. If there is a page with /404 as the route, then we just use that.  It's
-- the user deciding that they will control the 404.html directly.
-- 2. If there is not a page with a route /404:
--    a) Generate the Virtual 404 page with a default template of 404
--    b) Check that the template can be read.
--    c) If it's can't be read, then don't add the 404 page.
--    d) If it can, then add the 404 virtual page to the processing list
-- 3. Therefore:
--    a) If the user doesn't want a 404 page rendered; don't add a page with a
--    404 route and don't have a 404 template.
--    b) If the user just wants a template 404 page, but isn't interested in
--    customising it, then have a 404 template, but no page with a 404 route.
--    c) Have a custom 404 page (i.e. a page with a 404 route) and then use any
--    template you want; but you should specify it, otherwise it will probably
--    be 'default'.

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

import           Types.Errors           (SiteGenError (..))
import           Types.Header           (SourceMetadata (..))
import           Types.SiteGenState     (SiteGenReader (..), SiteGenState (..))

import           Lib.ResolvingTemplates (resolveTemplateName')
import           Lib.SiteGenConfig      (ConfigException, SiteGenConfig (..))
import           Lib.SiteGenState       (addToRenderList)




resolve404page
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
resolve404page = do
    m404 <- (pure . HashMap.lookup "/404") =<< PR.asks @SiteGenReader siteRouteMap
    -- if it already exists, then no need to do more, otherwise:
    when (isNothing m404) $ do
        let new404 = makeVSMFor404
        -- verify that the 404 template exists.
        mTname <- resolveTemplateName' (smTemplate new404)
        -- if it does then add the 404 page to the render list
        when (isJust mTname) $ do
            EL.logInfo "Adding default 404 handler."
            addToRenderList [new404]



makeVSMFor404 :: SourceMetadata
makeVSMFor404 = def { smRoute = "/404"
                    , smVimWikiLinkPath = "/404"
                    , smTitle = "Not Found"
                    , smTemplate = "404"
                    , smIndexPage = False
                    }
