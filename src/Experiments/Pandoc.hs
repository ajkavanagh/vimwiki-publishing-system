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

module Experiments.Pandoc where

import           TextShow

import           Control.Applicative   ((<|>))

-- global modules tests rely on
import qualified Text.Pandoc            as TP
import qualified Text.Pandoc.Builder    as B
import qualified Text.Pandoc.Definition as TPD
import qualified Text.Pandoc.Walk       as TPW

import           Data.Default           (def)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)
import           Data.Text              as T
import           Data.Text.Encoding     (decodeUtf8', encodeUtf8)
import           Data.ByteString        (ByteString)
import           Data.Maybe             (fromMaybe)

-- local modules to set up tests
import qualified Lib.Header             as H
import qualified Lib.SiteGenState       as SGS
import           Lib.SiteGenConfig      (SiteGenConfig)
import qualified Lib.SiteGenConfig      as SGC
import qualified Lib.Errors             as LE
import Lib.Errors             (SiteGenError)

-- module under test
import           Lib.PandocUtils        (parseMarkdown, processPandocAST,
                                         pandocToContentTextEither,
                                         pandocToSummaryTextEither,
                                         extractTocItemsToByteString,
                                         renderTocItemsToHtml,
                                         convertVimWikiLinks,
                                         processPandocLinks,
                                         findSummary, getSummaryPlain, getSummaryNPlain, getSummaryPandoc, getSummaryNPandoc,
                                         buildPandocFromTocItems, TocItem(..),
                                         dumpToc, extractToc, loadTocEither)


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

import           Effect.ByteStringStore (ByteStringStore)
import qualified Effect.ByteStringStore as EB
import           Effect.File (File)
import qualified Effect.File as EF

import Lib.SiteGenState     (SiteGenState, SiteGenReader)
import qualified Lib.SiteGenState     as SGS
import           Lib.SourceClass      (SourceContext(..))


-- parse a thing to a pandoc document (raw)


--helpers for tests
{-parse :: String -> TP.Pandoc-}
{-parse = B.doc . B.para . B.text-}

scContentM
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SourceContext
    -> Sem r Text
scContentM (VPC _)   = pure ""
scContentM (SPC spc) = fetchContentHtml spc


scSummaryM
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SourceContext
    -> Bool
    -> Sem r Text
scSummaryM (VPC _) _   = pure ""
scSummaryM (SPC spc) b = fetchSummaryHtml spc b


scTocM
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => SourceContext
    -> Maybe Int
    -> Sem r Text
scTocM (VPC _) _    = pure ""
scTocM (SPC spc) mi = fetchTocHtml spc mi


data ItemType = Content
              | SummaryRich
              | SummaryPlain
              | Toc
              deriving (Eq, Show)


-- parse a SourcePageContext content to pandoc and stash the content, summary
-- and toc elements
processSPCToByteStringStore
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> ItemType
    -> Sem r ByteString
processSPCToByteStringStore spc item = do
    CP.log @String $ "Parsing and processing: " <> H.spcRelFilePath spc
    bs <- EF.readFile (H.spcAbsFilePath spc) (Just $ H.spcHeaderLen spc) Nothing
    pd <- PE.fromEither $ parseMarkdown bs
    vws <- PR.asks @SiteGenReader SGS.siteVimWikiLinkMap
    maxSummaryWords <- PR.asks @SiteGenConfig SGC.sgcMaxSummaryWords
    let pd' = processPandocAST vws pd
    content <- PE.fromEither (encodeUtf8 <$> pandocToContentTextEither pd')
    (plain, rich) <- PE.fromEither (pairfmap encodeUtf8 <$> pandocToSummaryTextEither maxSummaryWords pd')
    let toc = extractTocItemsToByteString pd'
    -- now store the 4 parts in the ByteString store
    let key = T.pack $ H.spcRoute spc
    EB.store (key <> keySuffixFor Content) content
    EB.store (key <> keySuffixFor SummaryPlain) plain
    EB.store (key <> keySuffixFor SummaryRich) rich
    EB.store (key <> keySuffixFor Toc) toc
    CP.log @String $ "... completed: content, summary and toc for " <> H.spcRelFilePath spc
    pure $ case item of
        Content -> content
        SummaryPlain -> plain
        SummaryRich -> rich
        Toc -> toc


keySuffixFor :: ItemType -> Text
keySuffixFor Content      = "-content"
keySuffixFor SummaryPlain = "-summaryPlain"
keySuffixFor SummaryRich  = "-summaryRich"
keySuffixFor Toc          = "-toc"

pairfmap :: (a -> b) -> (a,a) -> (b,b)
pairfmap f (x,y) = (f x, f y)


-- | interface functions for fetching the dynamic content.  These are called via
-- the SourceClass class instance on the SourcePageContext.   We assume that
-- the required content is in the ByteStringStore, and if not, then we attempt
-- to process it above.  If band things happen then the Ginger file will not be
-- processed.

-- | generic helper to get the bytestring from the store, or process it into
-- existance and return it.
fetchStoreOrProcessAsBSFor
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> ItemType
    -> Sem r ByteString
fetchStoreOrProcessAsBSFor spc item = do
    let key = T.pack (H.spcRoute spc) <> keySuffixFor item
    mBs <- EB.fetch key
    case mBs of
        Nothing  -> processSPCToByteStringStore spc item
        Just bs' -> pure bs'


throwOrReturn :: (TextShow e, Member (Error SiteGenError) r) => Either e a -> Sem r a
throwOrReturn ex = case ex of
    Left e -> PE.throw $ LE.OtherError (showt e)
    Right x -> pure x


fetchContentHtml
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> Sem r Text
fetchContentHtml spc =
    throwOrReturn =<< decodeUtf8' <$> fetchStoreOrProcessAsBSFor spc Content



fetchSummaryHtml
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> Bool
    -> Sem r Text
fetchSummaryHtml spc isRich = do
    let item = if isRich then SummaryRich else SummaryPlain
    throwOrReturn =<< decodeUtf8' <$> fetchStoreOrProcessAsBSFor spc item


fetchTocHtml
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => H.SourcePageContext
    -> Maybe Int
    -> Sem r Text
fetchTocHtml spc mLevels = do
    tocItems <- throwOrReturn =<< loadTocEither <$> fetchStoreOrProcessAsBSFor spc Toc
    let levels = fromMaybe 6 mLevels
    case renderTocItemsToHtml levels tocItems of
        Left e -> PE.throw e
        Right html -> pure html
