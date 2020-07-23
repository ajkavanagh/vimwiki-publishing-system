{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
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
import           Data.Maybe             (fromJust, fromMaybe)
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
import qualified Text.Pandoc.Builder    as TPB
import qualified Text.Pandoc.Definition as TPD

import           Effect.Cache           (Cache)
import qualified Effect.Cache           as EC
import           Effect.File            (File)
import qualified Effect.File            as EF
import           Effect.Logging         (LoggingMessage)
import qualified Effect.Logging         as EL
import           Effect.Print           (Print)

import           Types.Errors           (SiteGenError)
import           Types.SiteGenState     (SiteGenReader, SiteGenState,
                                         siteVimWikiLinkMap)

import qualified Lib.Header             as H
import           Lib.SiteGenConfig      (SiteGenConfig)
import qualified Lib.SiteGenConfig      as SGC

import           Lib.PandocUtils        (extractToc, pandocToContentTextEither,
                                         pandocToSummaryTextEither,
                                         parseMarkdown, processPandocAST,
                                         renderTocItemsToHtml, stripMoreIndicator)


type PandocSemEffects r
  =    ( Member File r
       , Member (Cache Pandoc) r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log LoggingMessage) r
       , Member Print r
       )


-- | process the SPC to a pandoc AST and cache it/return it.  If it already
-- exists just return the cached version.
cachedProcessSMToPandocAST
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Sem r TP.Pandoc
cachedProcessSMToPandocAST sm = do
    let key = T.pack (H.smRoute sm) <> "-pandoc-ast"
    EC.fetch key >>= \case
        Just pd' -> pure pd'
        Nothing -> case H.smAbsFilePath sm of
            Nothing -> pure $ TPB.doc $ TPB.singleton TPD.Null
            Just absFile -> do
                EL.logInfo $ T.pack $ "Parsing and processing: " <> fromJust (H.smRelFilePath sm)
                bs <- EF.readFile absFile (Just $ H.smHeaderLen sm) Nothing
                pd <- PE.fromEither $ parseMarkdown bs
                vws <- PR.asks @SiteGenReader siteVimWikiLinkMap
                let pd'' = processPandocAST vws pd
                EC.store key pd''
                pure pd''



smContentM :: PandocSemEffects r => H.SourceMetadata -> Sem r Text
smContentM sm =
    PE.fromEither =<< pandocToContentTextEither . stripMoreIndicator <$> cachedProcessSMToPandocAST sm


smSummaryM
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Bool
    -> Sem r (Text, Bool)
smSummaryM sm isRich = do
    maxSummaryWords <- PR.asks @SiteGenConfig SGC.sgcMaxSummaryWords
    -- get Either err (plain, rick) as a Summary
    res <- pandocToSummaryTextEither maxSummaryWords <$> cachedProcessSMToPandocAST sm
    PE.fromEither $ fmap (if isRich then snd else fst) res


smTocM
    :: PandocSemEffects r
    => H.SourceMetadata
    -> Maybe Int
    -> Sem r Text
smTocM sm mLevels = do
    tocItems <- extractToc <$> cachedProcessSMToPandocAST sm
    let levels = fromMaybe 6 mLevels
    PE.fromEither $ renderTocItemsToHtml levels tocItems


markdownToHTML :: PandocSemEffects r => Text -> Sem r Text
markdownToHTML txt = do
    pd <- PE.fromEither $ parseMarkdown (encodeUtf8 txt)
    vws <- PR.asks @SiteGenReader siteVimWikiLinkMap
    let pd' = processPandocAST vws pd
    PE.fromEither $ pandocToContentTextEither pd'
