import           Test.Hspec        (Spec)
import           Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import           HeaderSpecs       (dropWithNewLineSpecs,
                                    findEndSiteGenHeaderSpecs, isHeaderSpecs,
                                    maybeExtractHeaderBlockSpecs,
                                    maybeDecodeHeaderSpecs)


main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs


specs :: Spec
specs = do
    isHeaderSpecs
    dropWithNewLineSpecs
    findEndSiteGenHeaderSpecs
    maybeExtractHeaderBlockSpecs
    maybeDecodeHeaderSpecs
