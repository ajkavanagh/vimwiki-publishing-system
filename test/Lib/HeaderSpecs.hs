{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.HeaderSpecs where

import           Data.ByteString   as BS
import qualified Data.ByteString   (ByteString)
import           Data.Default      (def)
import           Data.Function     ((&))
import           Data.Maybe        (fromMaybe)
import           Test.Hspec        (Spec, describe, it, pending, shouldBe, xit)
import           Text.RawString.QQ

-- Polysemy related
import           Colog.Polysemy    (runLogAsOutput)
import           Polysemy          (run)
import           Polysemy.Error    (Error, errorToIOFinal, runError, throw)
import           Polysemy.Output   (runOutputList)
import           Polysemy.Reader   (runReader)

-- The logging effect
import           Effect.Logging    (LoggingMessage)

-- Bits to get the tests to compile
import           Lib.Dates         (parseDate)
import           Lib.SiteGenConfig (SiteGenConfig (..))


-- module under test
import           Lib.Header        (HeaderContext (..), dropWithNewLine,
                                    findEndSiteGenHeader, isHeader,
                                    maybeDecodeHeader, maybeExtractHeaderBlock)
import           Types.Header      (SourceMetadata (..))



isHeaderSpecs :: Spec
isHeaderSpecs = --do

    describe "isHeader" $ do

        it "Should return false on empty string" $
            isHeader "" `shouldBe` False

        it "Should return false on string with now header start" $
            isHeader "Hello there" `shouldBe` False

        it "Should return True on a string with '--- sitegen'" $
            isHeader "--- sitegen    " `shouldBe` True

        it "Should return False on a string with '--- site that isn't a gen'" $
            isHeader "--- site that isn't a gen" `shouldBe` False


dropWithNewLineSpecs :: Spec
dropWithNewLineSpecs = --do

    describe "dropWithNewLine" $ do

        it "Should return (T.empty, len) for a string with no newline" $
            dropWithNewLine "hello" `shouldBe` ("", 5)

        it "Should return (T.empty, 0) for an empty string" $
            dropWithNewLine "" `shouldBe` ("", 0)

        it "Should find the newline and return the text after it" $
            dropWithNewLine "hello\nthere" `shouldBe` ("there", 6)

        it "Should find the first newline and return text after it" $
            dropWithNewLine "hello\r\nthere\nagain"
                `shouldBe` ("there\nagain", 7)


findEndSiteGenHeaderSpecs :: Spec
findEndSiteGenHeaderSpecs = --do


    describe "findEndSiteGenHeader" $ do

        it "Should return Nothing for empty string" $
            findEndSiteGenHeader "" `shouldBe` Nothing

        it "Should return Nothing is --- is not preceeded by a \\n" $
            findEndSiteGenHeader "hello---" `shouldBe` Nothing

        it "Should return before text (empty) and count 4 for just end header" $
            findEndSiteGenHeader "\n---" `shouldBe` Just ("", 4)

        it "Should return before text and count +4 for simple end header" $
            findEndSiteGenHeader "hello\n---" `shouldBe` Just ("hello", 9)

        it "Should return before text and count to final \\n" $
            findEndSiteGenHeader "hello\n--- this\n"
                `shouldBe` Just ("hello", 15)

        it "Should return multiline before text and proper count to final \\n" $
            findEndSiteGenHeader "h\ne\nllo\n--- this\n"
                `shouldBe` Just ("h\ne\nllo", 17)

        it "Should ignore text after final \\n" $
            findEndSiteGenHeader "h\ne\nllo\n--- this\nand"
                `shouldBe` Just ("h\ne\nllo", 17)


simpleHeader :: ByteString
simpleHeader = [r|--- sitegen
This is Line 1
This is Line 2
---
Done.
|]

-- This should not extract as it's empty ... i.e. not a header
emptyHeader :: ByteString
emptyHeader = [r|--- sitegen
---
Done.
|]

extractedHeader :: ByteString
extractedHeader = [r|This is Line 1
This is Line 2|]


maybeExtractHeaderBlockSpecs :: Spec
maybeExtractHeaderBlockSpecs = --do

    describe "maybeExtractHeaderBlock" $ do

        it "Should return (Nothing, 0) for an empty string" $
            maybeExtractHeaderBlock "" `shouldBe` (Nothing, 0)

        it "Should return the extracted header" $
            maybeExtractHeaderBlock simpleHeader
                `shouldBe` (Just extractedHeader, BS.length simpleHeader -6)

        it "Should return empty string and dropped length for emptyHeader" $
            maybeExtractHeaderBlock emptyHeader `shouldBe` (Nothing, 0)


{- \
    Test the Polysemy functions.  We need a runner that takes the things that
    the functions under test need and run them pure to get the result out.

    We need to test @maybeDecodeHeader@ so we'll have a test function that runs
    to completion.
-}


runMaybeDecodeHeader :: SiteGenConfig -> HeaderContext -> ByteString
                     -> ([LoggingMessage], Maybe SourceMetadata)
runMaybeDecodeHeader sgc rc txt =
    maybeDecodeHeader txt
        & runLogAsOutput @LoggingMessage
        & runOutputList @LoggingMessage
        & runReader sgc
        & runReader rc
        & run


defaultSCG :: SiteGenConfig
defaultSCG = SiteGenConfig
    { sgcSiteYaml="a/file/path"
    , sgcSiteUrl=Nothing
    , sgcSiteId="site1"
    , sgcRoot="a/file"
    , sgcVimWikiRoot="."
    , sgcSourceRelDir="."
    , sgcSource="src/"
    , sgcOutputDir="html/"
    , sgcExtension=".md"
    , sgcIndexPageName="index"
    , sgcThemeDir="theme/"
    , sgcTemplatesDirs=["theme/templates/"]
    , sgcTemplateExt=".html.j2"
    , sgcOutputFileExt=".html"
    , sgcStaticDirs=["static/"]
    , sgcCopyStaticFiles=False
    , sgcGenerateTags=True
    , sgcGenerateCategories=True
    , sgcGenerateFeed=True
    , sgcPublishDrafts=True
    , sgcIndexFiles=True
    , sgcMaxSummaryWords=70
    , sgcSkylightStyle=Nothing
    , sgcExtraDebug=True
    , sgcParams=Nothing
    }


defaultHC :: HeaderContext
defaultHC = HeaderContext
    { hcAutoSlug="auto/slug"
    , hcFileTime=fromMaybe undefined (parseDate "2020-03-28T09:00:00")
    , hcRelFilePath="some-name.md"
    , hcAbsFilePath="/some-name.md"
    , hcVimWikiLinkPath="some-name"
    , hcAutoTitle="auto-title"
    }


defaultSM :: SourceMetadata
defaultSM = def { smRoute="/auto/slug"
                , smRelFilePath=Just "some-name.md"
                , smAbsFilePath=Just "/some-name.md"
                , smVimWikiLinkPath="some-name"
                , smTitle="auto-title"
                , smTemplate="default"
                , smIndexPage=False
                , smAuthors=[]
                , smPublish=False
                , smComments=True
                , smSiteId="site1"
                , smHeaderLen=0 -- the length of the headerblock; i.e. what to drop to get to the content.
                }


minimalHeader :: ByteString
minimalHeader = [r|--- sitegen
title: This is the day
---
Done.
|]


fullHeader :: ByteString
fullHeader = [r|--- sitegen # this is the sitegen :)
route: the/route
title: The title
template: not-default
style: some-style.css
tags:
  - tag1
  - tag2
category: category1
date: 23-02-2010
updated: 30-04-2020 09:10:00
index-page: true
authors:
  - Alex Kavanagh
publish: true
site: default
---

# And the header
|]

fullHeaderSM :: SourceMetadata
fullHeaderSM = def { smRoute="/the/route"
                   , smRelFilePath=Just "some-name.md"
                   , smAbsFilePath=Just "/some-name.md"
                   , smVimWikiLinkPath="some-name"
                   , smTitle="The title"
                   , smTemplate="not-default"
                   , smTags=["tag1", "tag2"]
                   , smCategories=["category1"]
                   , smDate=parseDate "23-02-2010"
                   , smUpdated=parseDate "30-04-2020 09:10:00"
                   , smIndexPage=True
                   , smAuthors=["Alex Kavanagh"]
                   , smPublish=True
                   , smSiteId="default"
                   , smHeaderLen=0 -- the length of the headerblock; i.e. what to drop to get to the content.
                   , smParams=Nothing
                   }



maybeDecodeHeaderSpecs :: Spec
maybeDecodeHeaderSpecs = -- do

    describe "maybeDecodeHeader" $ do

        it "Should return (Nothing, 0) for an empty text" $
            runMaybeDecodeHeader defaultSCG defaultHC ""
                `shouldBe` ([], Nothing)

        it "Should return the title and length for a minimal header" $
            runMaybeDecodeHeader defaultSCG defaultHC minimalHeader
                `shouldBe` ([], Just (defaultSM { smTitle="This is the day"
                                                , smComments=False
                                                , smHeaderLen=39}))

        it "Should be 'Done.\\n' when dropping 39 chars from minimalHeader" $
            BS.drop 39 minimalHeader `shouldBe` "Done.\n"

        it "Should return the full SourceMetadata from a full config" $
            runMaybeDecodeHeader defaultSCG defaultHC fullHeader
                `shouldBe` ([], Just (fullHeaderSM { smHeaderLen=281
                                                   , smComments=False}))
