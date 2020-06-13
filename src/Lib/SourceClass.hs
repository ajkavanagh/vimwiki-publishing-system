--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings    #-}

-- for instance Source - remove if we don't do it that way!
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib.SourceClass where


import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe            (fromMaybe)

import qualified Lib.Header as H

-- The SourceClass module exports the Source class which has a number of methods
-- defined that are needed for rendering templates.  The idea is to paper over
-- the differences of SourcePageContext and VirtualIndexPageContext and any
-- other ones that come about.


-- | wrap up the concrete page contexts into a SourceContext
data SourceContext = SPC H.SourcePageContext
                   | VPC H.VirtualPageContext
                   deriving Show


class Source s where
    scRoute    :: s -> String
    scVimWikiLinkPath :: s -> String
    scAbsFilePath :: s -> Maybe String
    scRelFilePath :: s -> Maybe String


instance Source H.SourcePageContext where
    -- route    :: s -> String
    scRoute = H.spcRoute

    --vimWikiLinkPath :: s -> String
    scVimWikiLinkPath = H.spcVimWikiLinkPath

    -- absFilePath :: s -> Maybe String
    scAbsFilePath = Just . H.spcAbsFilePath

    --relFilePath :: s -> Maybe String
    scRelFilePath = Just . H.spcRelFilePath


instance Source H.VirtualPageContext where
    -- route    :: s -> String
    scRoute = H.vpcRoute

    --vimWikiLinkPath :: s -> String
    scVimWikiLinkPath = H.vpcVimWikiLinkPath

    -- absFilePath :: s -> Maybe String
    scAbsFilePath _ = Nothing

    --relFilePath :: s -> Maybe String
    scRelFilePath _ = Nothing


instance Source SourceContext where
    -- route    :: s -> String
    scRoute (SPC sc) = scRoute sc
    scRoute (VPC sc) = scRoute sc

    --vimWikiLinkPath :: s -> String
    scVimWikiLinkPath (SPC sc) = scVimWikiLinkPath sc
    scVimWikiLinkPath (VPC sc) = scVimWikiLinkPath sc

    -- absFilePath :: s -> Maybe String
    scAbsFilePath (SPC sc) = scAbsFilePath sc
    scAbsFilePath (VPC sc) = scAbsFilePath sc

    --relFilePath :: s -> Maybe String
    scRelFilePath (SPC sc) = scRelFilePath sc
    scRelFilePath (VPC sc) = scRelFilePath sc
