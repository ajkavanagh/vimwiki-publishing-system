{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}


{-# LANGUAGE AllowAmbiguousTypes  #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Experiments.Ginger where

import           TextShow

import qualified Data.ByteString.UTF8 as DBU
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
                                       SourceName, SourcePos, Template, ToGVal)
import qualified Text.Ginger          as TG
import           Text.Ginger.Html     (Html, htmlSource)

import           Effect.File          (File, FileException)
import qualified Effect.File          as EF

import           Lib.Errors           (GingerException (..))


-- what we want to do is to try and render something into the
-- index.html.j2 template
--
-- It has the following context variables:
--
-- * title
-- * navigation: [{url: 'xxx', label: 'xxx'},...]
-- * body
--
-- And this will render some sort of page.


-- parsing the template
--
-- We need to have some sort of resolver, that will map a filename -> input for
-- the Ginger to load

-- type IncludeResolver m = SourceName -> m (Maybe Source)
-- type Source = String
-- type SourceName = String

-- Somehow we want to run it in Sem r so we can get access to File.  i.e. the
-- 'm' above will be 'Sem r' so that the IncludeResolver m function can do
-- files.

includeResolver
    :: Members '[ File
                , Error FileException
                ] r
    => IncludeResolver (Sem r)
includeResolver source = do
    bs <- EF.readFile source Nothing Nothing
    pure $ Just $ DBU.toString bs


parseToTemplate
    :: Members '[ File
                , Error FileException
                , Error GingerException
                ] r
    => SourceName
    -> Sem r (Template SourcePos)
parseToTemplate source = do
    res <- TG.parseGingerFile includeResolver source
    case res of
        Left parseError -> PE.throw $ GingerException (T.pack $ show parseError)
        Right tpl       -> pure tpl


-- now let's render something

-- runGingerT
--     :: (ToGVal (Run p m h) h, ToGVal (Run p m h) p, Monoid h, Monad m, Applicative m, Functor m)
--     => GingerContext p m h
--     -> Template p
--     -> m (Either (RuntimeError p) (GVal (Run p m h)))

-- and

-- makeContextHtmlM
--     :: (Monad m, Functor m)
--     => (VarName -> Run p m Html (GVal (Run p m Html)))  -- function to look up things from a context
--     -> (Html -> m ())       -- function to take the output to something.
--     -> GingerContext p m Html
--
-- type VarName = Text    -- note it's TEXT not String!!
--
-- newtype Html = Html { unHtml :: Text }
-- i.e. this is Text wrapped in a Html wrapper type, using unHtml to extract it.
--
-- type Run p m h = ExceptT (RuntimeError p) (StateT (RunState p m h) (ReaderT (GingerContext p m h) m))
--
-- type Function m = [(Maybe Text, GVal m)] -> m (GVal m)
-- GVal m is a { ... } with data, and parameterised with m as a monad
-- class ToGVal m a where
--   toGVal :: a -> GVal m


-- get the html out (as text) -- we'll later push this to conduitT and write it
-- into a file
drainHtml
    :: Member (Writer Text) r
    => Html
    -> Sem r ()
drainHtml html = PW.tell $ htmlSource html


-- lookup.  At present, this is a HashMap, but we want it to end up as a
-- function that disambiguates to various things as needed.
contextLookup'
    :: forall k r b.
       ( Hashable k
       , Eq k
       , Show k
       --, Monad m
       , ToGVal (TG.Run TG.SourcePos (Sem r) Html) b
       , Member (Log String) r
       )
    => HashMap.HashMap k b
    -> k
    -> TG.Run TG.SourcePos (Sem r) Html (GVal (TG.Run TG.SourcePos (Sem r) Html))
--contextLookup' mm k = pure $ TG.toGVal $ HashMap.lookup k mm
contextLookup' mm k = do
    TG.liftRun $ CP.log @String $ "The key asked for was " ++ show k
    pure $ TG.toGVal $ HashMap.lookup k mm


-- now let's try to construct our render function that takes the template
renderTemplate
    :: forall r b.
       ( ToGVal (TG.Run TG.SourcePos (Sem r) Html) b
       , Member (Reader (HashMap.HashMap TG.VarName b)) r
       , Member (Writer Text) r
       , Member (Error GingerException) r
       , Member (Log String) r
       )
    => Template TG.SourcePos
    -> Sem r ()
renderTemplate tpl = do
    mm <- PR.ask @(HashMap.HashMap TG.VarName b)  -- get the hashmap
    let context = TG.makeContextHtmlM (contextLookup' mm) drainHtml
    res <- TG.runGingerT context tpl
    case res of
        Left err -> PE.throw $ GingerException (T.pack $ show err)
        Right _  -> pure ()



-- let's try to put it together now.
-- the template is './templates/index.html.j2'
-- the context variables are:
--  * title
--  * natigations [{url: '', label: ''}]
--  * body

---------------------------------------------------------------------------------
-- some test stuff to see if the functions work.
---------------------------------------------------------------------------------

-- It'll be curious to see how navigation and url/label are asked for, and I'd
-- better log those things

testData :: HashMap.HashMap TG.VarName Text
testData = HashMap.fromList [
    ("title", "The Title"),
    ("body", "The body")]


-- a test function that uses the testData and the template
renderTestData
    :: ( Member (Log String) r
       , Member (Error GingerException) r
       , Member File r
       , Member (Error FileException) r
       )
    => Sem r Text
renderTestData = do
    tpl <- parseToTemplate "./example-site/templates/index.html.j2"
    (o, _) <- PW.runWriterAssocR @Text $ PR.runReader testData $ renderTemplate tpl
    pure o


-- Run the Sem r monad to IO
runTest x = x & EF.fileToIO
              & PE.errorToIOFinal @FileException
              & PE.errorToIOFinal @GingerException
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal @IO



-- something I can just call
renderTestDataP = renderTestData & runTest
