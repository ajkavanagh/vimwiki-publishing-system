{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

-- for instance Source - remove if we don't do it that way!
--{-# LANGUAGE FlexibleInstances   #-}
--{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.Ginger where

import           Data.Function        ((&))
import           Data.Text              (Text)
import           Data.Text              as T
import qualified Data.ByteString.UTF8 as DBU

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
import           Polysemy.State      (State)
import qualified Polysemy.State      as PS

import           Text.Ginger          (GVal, IncludeResolver, Source,
                                       SourceName, SourcePos, Template, ToGVal)
import qualified Text.Ginger          as TG
import           Text.Ginger.Html     (Html, htmlSource)

import           Effect.Ginger        (GingerException (..))
import           Effect.File              (File, FileException)
import qualified Effect.File              as EF

import           Lib.Context.Core         (Context, RunSem, RunSemGVal, contextLookup)


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


includeResolver
    :: Members '[ File
                , Error FileException
                ] r
    => IncludeResolver (Sem r)
includeResolver source = do
    bs <- EF.readFile source Nothing Nothing
    pure $ Just $ DBU.toString bs


-- | Render a template using a context and a parsed Ginger template.  Note the
-- extra (Writer Text : ...) bit -- this is necessary as we want to run
-- PW.runWrirunWriterAssocR @Text in the body, and thus need to add that to the
-- 'r' bit as the @Context m@, but not @Sem r@ that comes into the function.
renderTemplate
    :: ( Member (Error GingerException) r
       , Member (Log String) r
       )
    => Context (RunSem (Writer Text : r))
    -> Template TG.SourcePos
    -> Sem r Text
renderTemplate ctxt tpl = do
    res <- PW.runWriterAssocR @Text $ renderTemplate' ctxt tpl
    pure $ fst res


renderTemplate'
    :: ( Member (Writer Text) r
       , Member (Error GingerException) r
       , Member (Log String) r
       )
    => Context (RunSem r)
    -> Template TG.SourcePos
    -> Sem r ()
renderTemplate' ctxt tpl = do
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
