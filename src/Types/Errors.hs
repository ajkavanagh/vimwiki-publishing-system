{-# LANGUAGE OverloadedStrings #-}

module Types.Errors
    where

import           Data.Text         (Text, pack)

import           Effect.File       (FileException (..))
import           Effect.Locale     (LocaleException (..))
import           Lib.SiteGenConfig (ConfigException (..))
import           Types.Ginger      (GingerException (..))
import           Types.Header      (SourceMetadata)

import qualified Text.Pandoc.Error as TPE


data SiteGenError
    = FileError FilePath Text       -- formed from FileException Filename error
    | GingerError Text            -- initially, this'll just be the text for the error
    | ConfigError Text Text       -- formed from Config Exception
    | LocaleError Text Text       -- if we get a locale error
    | SourceMetadataError SourceMetadata Text
    | PageDecodeError Text
    | PandocReadError TPE.PandocError
    | PandocWriteError TPE.PandocError
    | PandocProcessError Text
    | OtherError Text
    deriving (Show)


-- | Allows for custom error types to be coerced into the standard error resposne.
class IsSiteGenError e where
  mapSiteGenError :: e -> SiteGenError


instance IsSiteGenError FileException where
    mapSiteGenError (FileException fp etxt) = FileError fp etxt


instance IsSiteGenError GingerException where
    mapSiteGenError (GingerException txt) = GingerError txt


instance IsSiteGenError ConfigException where
    mapSiteGenError (ConfigException txt) = ConfigError "" txt


instance IsSiteGenError LocaleException where
    mapSiteGenError (LocaleException arg txt) =
        LocaleError (maybe "<none>" pack arg) txt
