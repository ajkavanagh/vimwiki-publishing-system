import           Test.Hspec           (Spec)
import           Test.Hspec.Runner    (configFastFail, defaultConfig, hspecWith)

import           Lib.HeaderSpecs      (dropWithNewLineSpecs,
                                       findEndSiteGenHeaderSpecs, isHeaderSpecs,
                                       maybeDecodeHeaderSpecs,
                                       maybeExtractHeaderBlockSpecs)
import           Lib.PandocUtilsSpecs (processPandocLinksSpecs)
import           Lib.RouteUtilsSpecs  (checkDuplicateRoutesSpecs)



main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs


specs :: Spec
specs = do
    -- HeaderSpecs
    isHeaderSpecs
    dropWithNewLineSpecs
    findEndSiteGenHeaderSpecs
    maybeExtractHeaderBlockSpecs
    maybeDecodeHeaderSpecs

    -- PandocUtilsSpecs
    processPandocLinksSpecs

    -- checking route utils
    checkDuplicateRoutesSpecs
