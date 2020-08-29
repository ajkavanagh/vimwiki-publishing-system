{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Lib.BuiltInTemplates
    ( textForTemplate
    ) where


import Data.ByteString  (ByteString)
import Data.FileEmbed (embedStringFile)

import Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Types.Constants


type BuiltInTemplateMap = HashMap String ByteString


feedTemplate :: ByteString
feedTemplate = $(embedStringFile "templates/feed.atom")


buildInTemplateMap = HashMap.fromList
    [ (atomFeedTemplate, feedTemplate)
    ]


textForTemplate :: String -> Maybe ByteString
textForTemplate = flip HashMap.lookup buildInTemplateMap
