{-# LANGUAGE OverloadedStrings #-}

module Lib.RouteUtilsSpecs where

import           Test.Hspec     (Spec, describe, it, pending, shouldBe)

-- global modules tests rely on
import           Data.Default   (def)

-- local modules to set up tests
import           Lib.Errors     (SiteGenError (..))
import qualified Lib.Header     as H

-- module under test
import qualified Lib.RouteUtils as RU


checkDuplicateRoutesSpecs :: Spec
checkDuplicateRoutesSpecs = --do

    describe "checkDuplicateRoutesSpecs" $ do

        it "Should do nothing with an empty list" $
            RU.checkDuplicateRoutes [] `shouldBe` []

        it "Should provide an empty list with no duplicates" $
            RU.checkDuplicateRoutes [s1, s2, s3] `shouldBe` []

        it "Should indicate the duplicate with 2 errors" $
            RU.checkDuplicateRoutes [s1, s2, d1, s3] `shouldBe` [e1,e2]



s1 :: H.SourcePageHeader
s1 = def {H.phRoute="r1", H.phRelFilePath="f1"}


s2 :: H.SourcePageHeader
s2 = def {H.phRoute="r2", H.phRelFilePath="f2"}


s3 :: H.SourcePageHeader
s3 = def {H.phRoute="r3", H.phRelFilePath="f3"}


d1 :: H.SourcePageHeader
d1 = def {H.phRoute="r1", H.phRelFilePath="d1"}


-- errors
e1 = PageError s1 "Pages share same route: \"r1\", filenames: f1, d1"
e2 = PageError d1 "Pages share same route: \"r1\", filenames: f1, d1"
