{-# LANGUAGE OverloadedStrings   #-}

module Lib.Errors
    where

import Data.Text (Text)

import           Effect.File       (FileException(..))
import           Effect.Ginger     (GingerException(..))
import           Lib.SiteGenConfig (ConfigException(..))



data SiteGenError
    = FileError FilePath Text       -- formed from FileException Filename error
    | GingerError Text            -- initially, this'll just be the text for the error
    | ConfigError Text Text       -- formed from Config Exception
--  | PandocError PandocException
    deriving Show


-- | Allows for custom error types to be coerced into the standard error resposne.
class IsSiteGenError e where
  mapSiteGenError :: e -> SiteGenError


instance IsSiteGenError FileException where
    mapSiteGenError (FileException fp etxt) = FileError fp etxt


instance IsSiteGenError GingerException where
    mapSiteGenError (GingerException txt) = GingerError txt


instance IsSiteGenError ConfigException where
    mapSiteGenError (ConfigException txt) = ConfigError "" txt
