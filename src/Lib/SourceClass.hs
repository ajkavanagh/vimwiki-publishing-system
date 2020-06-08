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


class SourceM s m where
    scContentM  :: Monad m => s -> m Text
    scSummaryM  :: Monad m => s -> m Text
    scCategoryM :: Monad m => s -> m Text
    scTagsM     :: Monad m => s -> m [Text]
    scRouteM    :: Monad m => s -> m String


instance SourceM H.SourcePageContext m where
    -- content :: Monad m => s -> m Text
    scContentM spc = pure ""

    -- summary :: Monad m => s -> m Text
    scSummaryM spc = pure ""

    -- category :: Monad m => s -> m Text
    scCategoryM spc = pure $ T.pack $ fromMaybe "" $ H.spcCategory spc

    -- tags     :: Monad m => s -> m [Text]
    scTagsM spc = pure $ map T.pack $ H.spcTags spc

    -- route    :: Monad m => s -> m String
    scRouteM spc = pure $ H.spcRoute spc


instance SourceM H.VirtualPageContext m where
    -- content :: Monad m => s -> m Text
    scContentM _ = pure ""

    -- summary :: Monad m => s -> m Text
    scSummaryM _ = pure ""

    -- category :: Monad m => s -> m Text
    scCategoryM _ = pure ""

    -- tags     :: Monad m => s -> m [Text]
    scTagsM _ = pure []

    -- route    :: Monad m => s -> m String
    scRouteM vpc = pure $ H.vpcRoute vpc



-- | wrap up the concrete page contexts into a SourceContext
data SourceContext = SPC H.SourcePageContext
                   | VPC H.VirtualPageContext
                   deriving Show


instance Source SourceContext where
    -- route    :: s -> String
    scRoute (SPC sc) = H.spcRoute sc
    scRoute (VPC sc) = H.vpcRoute sc

    --vimWikiLinkPath :: s -> String
    scVimWikiLinkPath (SPC sc) = H.spcVimWikiLinkPath sc
    scVimWikiLinkPath (VPC sc) = H.vpcVimWikiLinkPath sc

    -- absFilePath :: s -> Maybe String
    scAbsFilePath (SPC sc) = Just $ H.spcAbsFilePath sc
    scAbsFilePath (VPC _) = Nothing

    --relFilePath :: s -> Maybe String
    scRelFilePath (SPC sc) = Just $ H.spcRelFilePath sc
    scRelFilePath (VPC _) = Nothing
