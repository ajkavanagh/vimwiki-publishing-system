{-# LANGUAGE OverloadedStrings #-}

module Lib.PandocUtilsSpecs where

import           Test.Hspec             (Spec, describe, it, pending, shouldBe)

-- global modules tests rely on
import qualified Text.Pandoc            as TP
import qualified Text.Pandoc.Builder    as B
import qualified Text.Pandoc.Definition as TPD
import qualified Text.Pandoc.Walk       as TPW

import           Data.Default           (def)
import           Data.Either            (either)
import qualified Data.HashMap.Strict    as HashMap
import           Data.Text              (Text)

-- local modules to set up tests
import qualified Lib.Header             as H
import qualified Types.SiteGenState     as SGS


-- module under test
import           Lib.PandocUtils        (convertVimWikiLinks,
                                         processPandocLinks)


processPandocLinksSpecs :: Spec
processPandocLinksSpecs = --do

    describe "processPandocLinksSpecs" $ do

        it "Should ignore no links at all" $
            runTest HashMap.empty (parse "This has no link")
                `shouldBe` "<p>This has no link</p>"

        it "Should remove a link with no SPC" $
            runTest HashMap.empty (parse "This one [[does]] have a link.")
                `shouldBe` "<p>This one does have a link.</p>"

        it "Should replace a simple link" $
            runTest simpleMap (parse "This one [[does]] have a link.")
                `shouldBe` "<p>This one <a href=\"new-route\" title=\"does\">does</a> have a link.</p>"

        it "Should replace the 'does' link and remove the hello link" $
            runTest simpleMap (parse "This [[does]] link is great, but [[two|hello]] is better")
                `shouldBe` ("<p>This <a href=\"new-route\" title=\"does\">does</a> link is great" <>
                            ", but hello is better</p>")

        it "Should not detect a uri link in text" $
            runTest simpleMap (parse "this http://example.com should stay as text")
                `shouldBe` "<p>this http://example.com should stay as text</p>"

        it "Should not remove a wiki uri link" $
            runTest simpleMap (parse "this [[http://example.com|link]] should be a link")
                `shouldBe` "<p>this <a href=\"http://example.com\" title=\"link\">link</a> should be a link</p>"

        it "Should work with two links" $
            runTest simpleMap (parse  "This [[link]] and that [[link1|Description]].")
                `shouldBe` ("<p>This <a href=\"new-route\" title=\"link\">link</a> and that " <>
                            "<a href=\"new-route2\" title=\"Description\">Description</a>.</p>")

        it "Should lowercase relative links to routes" $
            runTest simpleMap (parse "This [[Link]] should be converted.")
                `shouldBe` "<p>This <a href=\"new-route\" title=\"Link\">Link</a> should be converted.</p>"

        it "Should replace a link with a fragment in it" $
            runTest simpleMap (parse "This [[Link#segment]] should be converted.")
                `shouldBe` ("<p>This <a href=\"new-route#segment\" " <>
                            "title=\"Link#segment\">Link#segment</a>" <>
                            " should be converted.</p>")

        it "Should replace a link/description with a fragment in it" $
            runTest simpleMap (parse "This [[Link#segment|Thing]] should be converted.")
                `shouldBe` ("<p>This <a href=\"new-route#segment\" " <>
                            "title=\"Thing\">Thing</a>" <>
                            " should be converted.</p>")

--
-- a SGS.VimWikiLinkToSC map with just link to re-write it to a route
linkSPC :: H.SourcePageContext
linkSPC = def {H.spcRoute="new-route"}


simpleMap :: SGS.VimWikiLinkToSC
simpleMap = HashMap.fromList
    [ ("does", H.SPC linkSPC)
    , ("link", H.SPC linkSPC)
    , ("link1", H.SPC $ def {H.spcRoute="new-route2"})
    ]

--helpers for tests
parse :: Text -> TP.Pandoc
parse = B.doc . B.para . B.text


runProcessPandocLinks :: SGS.VimWikiLinkToSC -> TP.Pandoc -> TP.Pandoc
runProcessPandocLinks hmap = TPW.walk (processPandocLinks hmap)


-- convert the wikilinks and then see if we should re-write them
process :: SGS.VimWikiLinkToSC -> TP.Pandoc -> TP.Pandoc
process hmap doc = runProcessPandocLinks hmap $ convertVimWikiLinks doc


runTest :: SGS.VimWikiLinkToSC -> TP.Pandoc -> Text
runTest hmap x =
    either (error . show) id
           (TP.runPure (TP.writeHtml5String TP.def (process hmap x)))
