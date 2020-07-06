{-# LANGUAGE ConstraintKinds      #-}
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
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.Pandoc where

import           TextShow

import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8', encodeUtf8)

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP

import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import qualified Polysemy.Error         as PE
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)

import           Text.Pandoc            (Pandoc)
import qualified Text.Pandoc            as TP

import           Effect.File            (File)
import qualified Effect.File            as EF
import           Effect.Cache           (Cache)
import qualified Effect.Cache           as EC
import           Effect.Print           (Print)
import           Effect.Logging         (LoggingMessage)
import qualified Effect.Logging         as EL

import           Lib.Errors             (SiteGenError)
import qualified Lib.Errors             as LE
import qualified Lib.Header             as H
import           Lib.SiteGenConfig      (SiteGenConfig)
import qualified Lib.SiteGenConfig      as SGC
import           Types.SiteGenState     (SiteGenReader, SiteGenState,
                                         siteVimWikiLinkMap)

import           Lib.PandocUtils        (pandocToContentTextEither,
                                         pandocToSummaryTextEither,
                                         parseMarkdown, processPandocAST,
                                         extractToc, renderTocItemsToHtml)


type PandocSemEffects r
  =    ( Member File r
       , Member (Cache Pandoc) r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       , Member (Log LoggingMessage) r
       , Member Print r
       )


scContentM :: PandocSemEffects r => H.SourceContext -> Sem r Text
scContentM (H.VPC _)   = pure ""
scContentM (H.SPC spc) = fetchContentHtml spc


scSummaryM :: PandocSemEffects r => H.SourceContext -> Bool -> Sem r Text
scSummaryM (H.VPC _) _   = pure ""
scSummaryM (H.SPC spc) b = fetchSummaryHtml spc b


scTocM :: PandocSemEffects r => H.SourceContext -> Maybe Int -> Sem r Text
scTocM (H.VPC _) _    = pure ""
scTocM (H.SPC spc) mi = fetchTocHtml spc mi


-- | process the SPC to a pandoc AST and cache it/return it.  If it already
-- exists just return the cached version.
cachedProcessSPCToPandocAST
    :: PandocSemEffects r
    => H.SourcePageContext
    -> Sem r TP.Pandoc
cachedProcessSPCToPandocAST spc = do
    let key = T.pack (H.spcRoute spc) <> "-pandoc-ast"
    EC.fetch key >>= \case
        Just pd' -> pure pd'
        Nothing -> do
            EL.logInfo $ T.pack $ "Parsing and processing: " <> H.spcRelFilePath spc
            bs <- EF.readFile (H.spcAbsFilePath spc) (Just $ H.spcHeaderLen spc) Nothing
            pd <- PE.fromEither $ parseMarkdown bs
            vws <- PR.asks @SiteGenReader siteVimWikiLinkMap
            let pd'' = processPandocAST vws pd
            EC.store key pd''
            pure pd''



fetchContentHtml :: PandocSemEffects r => H.SourcePageContext -> Sem r Text
fetchContentHtml spc =
    PE.fromEither =<< pandocToContentTextEither <$> cachedProcessSPCToPandocAST spc


fetchSummaryHtml
    :: PandocSemEffects r
    => H.SourcePageContext
    -> Bool
    -> Sem r Text
fetchSummaryHtml spc isRich = do
    maxSummaryWords <- PR.asks @SiteGenConfig SGC.sgcMaxSummaryWords
    -- get Either err (plain, rick) as a Summary
    res <- pandocToSummaryTextEither maxSummaryWords <$> cachedProcessSPCToPandocAST spc
    PE.fromEither $ fmap (if isRich then snd else fst) res


fetchTocHtml
    :: PandocSemEffects r
    => H.SourcePageContext
    -> Maybe Int
    -> Sem r Text
fetchTocHtml spc mLevels = do
    tocItems <- extractToc <$> cachedProcessSPCToPandocAST spc
    let levels = fromMaybe 6 mLevels
    PE.fromEither $ renderTocItemsToHtml levels tocItems


markdownToHTML :: PandocSemEffects r => Text -> Sem r Text
markdownToHTML txt = do
    pd <- PE.fromEither $ parseMarkdown (encodeUtf8 txt)
    vws <- PR.asks @SiteGenReader siteVimWikiLinkMap
    let pd' = processPandocAST vws pd
    PE.fromEither $ pandocToContentTextEither pd'
