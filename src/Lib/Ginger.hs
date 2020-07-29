{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.Ginger where

import           System.FilePath.Posix  (FilePath, (<.>))

import qualified Data.ByteString.UTF8   as DBU
import           Data.Text              (Text)
import           Data.Text              as T

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP
import           Polysemy               (Member, Members, Sem)
import           Polysemy.Error         (Error)
import qualified Polysemy.Error         as PE
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)
import           Polysemy.Writer        (Writer)
import qualified Polysemy.Writer        as PW

import           Text.Ginger            (IncludeResolver, SourceName, SourcePos,
                                         Template)
import qualified Text.Ginger            as TG
import           Text.Ginger.Html       (Html, htmlSource)

import           Effect.Cache           (Cache)
import qualified Effect.Cache           as EC
import           Effect.File            (File, FileException)
import qualified Effect.File            as EF
import           Effect.Logging         (LoggingMessage)
import qualified Effect.Logging         as EL

import           Types.Context          (Context, RunSem, RunSemGVal)
import           Types.Errors           (SiteGenError)
import           Types.Ginger           (GingerException (..))
import           Types.SiteGenState     (SiteGenReader, SiteGenState)

import           Lib.Context.Core       (contextLookup)
import           Lib.ResolvingTemplates (resolveTemplatePath)
import           Lib.SiteGenConfig      (ConfigException, SiteGenConfig (..))


-- | parseToTemplate takes a sourcename (filename basically) and resolves it to
-- a Ginger parsed template.  This is then cached for quicker lookups when the
-- template is nexted asked for.
parseToTemplate
    :: Members '[ File
                , Error FileException
                , Error GingerException
                , Error SiteGenError
                , Error ConfigException
                , Reader SiteGenConfig
                , Reader SiteGenReader
                , State SiteGenState
                , Cache (Template SourcePos)
                , Log LoggingMessage
                ] r
    => SourceName
    -> Sem r (Template SourcePos)
parseToTemplate source = do
    mTpl <- EC.fetch (T.pack source)
    case mTpl of
        Just tpl' -> do
            EL.logInfo $ T.pack $ "  * Using cached template: " <> source
            pure tpl'
        Nothing -> do
            EL.logInfo $ T.pack $ "  * Parsing template: " <> source
            res <- TG.parseGingerFile includeResolver source
            case res of
                Left parseError -> PE.throw $ GingerException (T.pack $ show parseError)
                Right tpl       -> do
                    EC.store (T.pack source) tpl
                    pure tpl


includeResolver
    :: Members '[ File
                , Error FileException
                , Error ConfigException
                , Error SiteGenError
                , Reader SiteGenConfig
                , Reader SiteGenReader
                , State SiteGenState
                , Log LoggingMessage
                ] r
    => IncludeResolver (Sem r)
includeResolver source = do
    EL.logDebug $ T.pack $ "includeResolver: trying to resolve :" <> show source
    -- try using the filepath we were sent
    sgc <- PR.ask @SiteGenConfig
    let tDirs = sgcTemplatesDirs sgc
        tExt  = sgcTemplateExt sgc
    mFp <- resolveTemplatePath tDirs source >>= (\case
        -- if we got nothing back, try to resolve it with an extension added
        Nothing -> resolveTemplatePath tDirs (source <.> tExt)
        fp@(Just _) -> pure fp)
    case mFp of
        Just fp -> Just . DBU.toString <$> EF.readFile fp Nothing Nothing
        Nothing -> PE.throw $ EF.FileException source "File Not found"


-- | Render a template using a context and a parsed Ginger template.  Note the
-- extra (Writer Text : ...) bit -- this is necessary as we want to run
-- PW.runWrirunWriterAssocR @Text in the body, and thus need to add that to the
-- 'r' bit as the @Context m@, but not @Sem r@ that comes into the function.
renderTemplate
    :: ( Member (Error GingerException) r
       , Member (Log LoggingMessage) r
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
       , Member (Log LoggingMessage) r
       )
    => Context (RunSem r)
    -> Template TG.SourcePos
    -> Sem r ()
renderTemplate' ctxt tpl = do
    let context = TG.makeContextHtmlM (contextLookup ctxt) drainHtml
    res <- TG.runGingerT context tpl
    case res of
        Left err -> PE.throw $ GingerException (T.pack $ show err)
        Right _  -> pure ()


-- get the html out (as text) -- we'll later push this to conduitT and write it
-- into a file
drainHtml
    :: Member (Writer Text) r
    => Html
    -> Sem r ()
drainHtml html = PW.tell $ htmlSource html
