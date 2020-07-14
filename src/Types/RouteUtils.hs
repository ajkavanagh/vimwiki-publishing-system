module Types.RouteUtils where

import qualified Data.Text    as T


import           Types.Header (SourceMetadata (..))


data RouteError = DuplicateRouteError SourceMetadata T.Text
                  deriving (Eq, Show)

