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

module Experiments.Context where

import qualified Data.ByteString.UTF8 as DBU
import           Data.Default         (def)
import           Data.Function        ((&))
import           Data.Hashable        (Hashable)
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HashMap
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Colog.Core           (logStringStderr)
import           Colog.Polysemy       (Log, runLogAction)
import qualified Colog.Polysemy       as CP
import           Polysemy             (Embed, Member, Members, Sem, embed,
                                       embedToFinal, interpret, makeSem, run,
                                       runFinal)
import           Polysemy.Error       (Error)
import qualified Polysemy.Error       as PE
import           Polysemy.Reader      (Reader)
import qualified Polysemy.Reader      as PR
import           Polysemy.Writer      (Writer)
import qualified Polysemy.Writer      as PW

import           Text.Ginger          (GVal, IncludeResolver, Source,
                                       SourceName, SourcePos, Template, ToGVal,
                                       (~>))
import qualified Text.Ginger          as TG
import           Text.Ginger.Html     (Html, htmlSource)

import           Effect.File          (File, FileException)
import qualified Effect.File          as EF

import           Effect.Ginger        (GingerException (..))

import qualified Lib.SiteGenConfig    as S

import           Experiments.Ginger   (parseToTemplate)

{-
   In English, I want a thing that can store bits of Text and when asked with that
   bit of text, respond with a function that can then be run in the context of a
   Monad m, to give as a Gval m.  I'm not even sure if this is possible!

   So an @m (GVal m)@ is a thing that needs to be evalutated in the context on m.

-}

-- This is the type that Ginger runs for Sem r when doing context lookups.
type RunSem r = TG.Run TG.SourcePos (Sem r) Html
type RunSemGVal r = TG.Run TG.SourcePos (Sem r) Html (GVal (TG.Run TG.SourcePos (Sem r) Html))

newtype Context m = Context { unContext :: Monad m => HashMap.HashMap Text (m (GVal m)) }

emptyContext :: Monad m => Context m
emptyContext = Context HashMap.empty


-- okay, the idea is to simulate tags['tag1'].pages
-- which is  "resolve 'tags'" then lookup 'tag1' the lookup 'pages' and return
-- whatever that is".  At each point, the asLookup on a GVal m needs to be
-- called.
--
-- It turns out there is no way to do this.  So, we have to fall back to another
-- mechanism, which is to use the @Function m@ type which is helfully:
-- type Function m = [(Maybe Text, GVal m)] -> m (GVal m)
-- i.e. it runs inside the monad.
--
-- Thus, we'll have a generic function 'getComputed' that takes a route and a
-- string to represent what is wanted, and thus will be able to run inside the
-- Sem r monad to get the information that is needed during template generation.
-- Thus we can process summary's lazily so we don't have to do everything up
-- front.  What's that going to look like?


-- registerIntoContext :: Context m -> Context m

-- store a function that is used to resolve to a GVal m against a Text key
registerContextKeyGVal :: Monad m => Context m -> Text -> m (GVal m) -> Context m
registerContextKeyGVal ctxt key f = Context $ HashMap.insert key f (unContext ctxt)


resolveContext :: Monad m => Context m -> Text -> Maybe (m (GVal m))
resolveContext ctxt k = HashMap.lookup k $ unContext ctxt


{-
   So how do we do key1.key2 lookups?

   key1 -> GVal, which Ginger will then do a lookup on that.  How do we arrange
   things so that we still get the 'call' to resolve the 2nd lookup?  We'll have
   to have a class that has a toGVal instance; but will that continue to run in
   the monad?

  Unfortunately, we can't: the context is supplied as a ReaderT which means that
  it's static, and evaluated when the runReaderT is evaluated; and this is part
  of the runGingerT:

  runGingerT context tpl =
    runReaderT (evalStateT (runExceptT (runTemplate tpl)) (defRunState tpl)) context

  i.e. 'context' is the thing that we are trying to resolve to.

  So at best, we can dynamically resolve the first level, but after that it needs
  to be a static GVal.
-}


-- lookup the context value in the Context m hashmap.  The HashMap is in the
-- (Sem r) monad, but this function can run in the (Run p m h) monad which can
-- be built in to the renderer.
contextLookup
    :: forall r.
       ( Member (Log String) r
       )
    => Context (RunSem r)
    -> Text
    -> RunSemGVal r
contextLookup ctxt key = do
    TG.liftRun $ CP.log @String $ "The key asked for was " ++ show key
    case resolveContext ctxt key of
        Nothing -> do
            TG.liftRun $ CP.log @String "key was not resolved!"
            pure def
        Just f' -> f'


-- so we want to provide the SiteGenConfig structure as a (GVal m) under the
-- key 'sitegen'.  This means we need to use the reader for it, and then convert
-- it into a HashMap kind of thing with sensible keys to actually

siteGenMGValM
    :: Member (Reader S.SiteGenConfig) r
    => RunSemGVal r
siteGenMGValM = do
    sgc <- TG.liftRun $ PR.ask @S.SiteGenConfig
    pure $ TG.dict
        [ "siteYaml" ~> S.sgcSiteYaml sgc
        , "siteId" ~> S.sgcSiteId sgc
        , "source" ~> S.sgcSource sgc
        , "outputDir" ~> S.sgcOutputDir sgc
        , "extension" ~> S.sgcExtension sgc
        , "indexPageName" ~> S.sgcIndexPageName sgc
        , "templatesDir" ~> S.sgcTemplatesDir sgc
        , "templateExt" ~> S.sgcTemplateExt sgc
        , "staticDir" ~> S.sgcStaticDir sgc
        , "generateTags" ~> S.sgcGenerateTags sgc
        , "generateCategories" ~> S.sgcGenerateCategories sgc
        , "publishDrafts" ~> S.sgcPublishDrafts sgc
        ]


registerIntoContext
    :: ( Member (Reader S.SiteGenConfig) r
       , Member (Log String) r
       )
    => Context (RunSem r) -> Context (RunSem r)
registerIntoContext ctxt =
    Prelude.foldr ($) ctxt [ registerSiteGenIntoContext
                           , registerBodyIntoContext
                           , registerTitleIntoContext
                           , registerDynamicLookupIntoContext
                           , registerThingIntoContext
                           ]

registerDynamicLookupIntoContext
    :: Member (Log String) r
    => Context (RunSem r) -> Context (RunSem r)
registerDynamicLookupIntoContext ctxt =
    registerContextKeyGVal ctxt "getValue" (pure $ TG.fromFunction dynamicLookup)

--
-- let's do a function that does dynamic lookup of the a variable; we'll just
-- want to log what it does so we can see the form of the variables.
-- type Function m = [(Maybe Text, GVal m)] -> m (GVal m)
dynamicLookup :: Member (Log String) r => TG.Function (RunSem r)
dynamicLookup args = do
    TG.liftRun $ CP.log @String $ "Args are " ++ show args
    pure $ TG.toGVal "from dynamicLookup"


registerSiteGenIntoContext
    :: Member (Reader S.SiteGenConfig) r
    => Context (RunSem r) -> Context (RunSem r)
registerSiteGenIntoContext ctxt = registerContextKeyGVal ctxt "sitegen" siteGenMGValM


valueInMonad :: (Monad m, ToGVal m v) => v -> m (GVal m)
valueInMonad val = pure $ TG.toGVal val


registerBodyIntoContext
    :: Monad m
    => Context m -> Context m
registerBodyIntoContext ctxt = registerContextKeyGVal ctxt "body" (valueInMonad "The Body Text")


registerTitleIntoContext
    :: Monad m
    => Context m -> Context m
registerTitleIntoContext ctxt = registerContextKeyGVal ctxt "title" (valueInMonad "The title")


registerThingIntoContext :: Member (Log String) r => Context (RunSem r) -> Context (RunSem r)
registerThingIntoContext ctxt = registerContextKeyGVal ctxt "thing" thingMGValM


thingMGValM :: Member (Log String) r => RunSemGVal r
thingMGValM = pure $ TG.dict [("func", TG.fromFunction dynamicLookup)]

------

-- THIS IS ALL TEST FUNCTIONS:
--
-- and now some testing to see if it will go into a rendering system

-- now let's try to construct our render function that takes the template
renderTemplate
    :: forall r.
       ( Member (Writer Text) r
       , Member (Error GingerException) r
       , Member (Log String) r
       )
    => Context (RunSem r)
    -> Template TG.SourcePos
    -> Sem r ()
renderTemplate ctxt tpl = do
    let context = TG.makeContextHtmlM (contextLookup ctxt) drainHtml
    res <- TG.runGingerT context tpl
    case res of
        Left err -> PE.throw $ GingerException (T.pack $ show err)
        Right t  -> do
            CP.log @String $ "Output was: " ++ show t
            pure ()


-- get the html out (as text) -- we'll later push this to conduitT and write it
-- into a file
drainHtml
    :: Member (Writer Text) r
    => Html
    -> Sem r ()
drainHtml html = PW.tell $ htmlSource html



-- now try to tie it all together

makeTheContextRunSemR
    :: ( Member (Reader S.SiteGenConfig) r
       , Member (Log String) r
       )
    => Context (RunSem r)
makeTheContextRunSemR = registerIntoContext emptyContext


-- a test function that uses the testData and the template
renderTestContext
    :: ( Member (Log String) r
       , Member (Error GingerException) r
       , Member File r
       , Member (Error FileException) r
       , Member (Reader S.SiteGenConfig) r
       , Member (Writer Text) r
       )
    => Sem r ()
renderTestContext = do
    tpl <- parseToTemplate "./example-site/templates/index.html.j2"
    let ctxt = makeTheContextRunSemR
    renderTemplate ctxt tpl


getSiteGenConfig
    :: Members '[ Log String
                , File
                , Error FileException
                , Error S.ConfigException ] r
    => Sem r S.SiteGenConfig
getSiteGenConfig = do
    let fp = "./example-site/site.yaml"
        draft = True
    S.getSiteGenConfig fp draft


-- Run the Sem r monad to IO
runTestC x = x & EF.fileToIO
               & PE.errorToIOFinal @FileException
               & PE.errorToIOFinal @GingerException
               & PE.errorToIOFinal @S.ConfigException
               & PW.runWriterAssocR @Text
               & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
               & embedToFinal @IO
               & runFinal @IO

runSGC x = x & EF.fileToIO
             & PE.errorToIOFinal @FileException
             & PE.errorToIOFinal @GingerException
             & PE.errorToIOFinal @S.ConfigException
             & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
             & embedToFinal @IO
             & runFinal @IO

fromRight :: Show a => Either a b -> b
fromRight (Right x) = x
fromRight (Left y)  = error (show y)

-- something I can just call
renderTestContextP = do
    sgc <- getSiteGenConfig & runSGC
    let sgc' = fromRight $ fromRight $ fromRight sgc
    PR.runReader @S.SiteGenConfig sgc' renderTestContext & runTestC
