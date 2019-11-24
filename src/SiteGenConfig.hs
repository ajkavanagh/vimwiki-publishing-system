{-# LANGUAGE OverloadedStrings #-}

module SiteGenConfig where

import System.Directory ( doesFileExist
                        , doesDirectoryExist
                        , makeAbsolute
                        )
import System.FilePath        (FilePath, takeDirectory, pathSeparator, (</>))

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception.Safe (Exception, MonadThrow, SomeException, throwM, catch, try)
import Data.Typeable          (TypeRep, Typeable, typeRep)

import Data.Yaml
import Data.Maybe             (fromJust, isNothing)


data ConfigException = ConfigException String
                     | ConfigExceptions [ConfigException]
                      deriving (Show, Typeable)

instance Exception ConfigException

{-
site: <site-identifier>
source: ./src  # the directory (relative, if wanted, to the site.yaml) to start
               # looking for sitegen pages.
output-dir: ./html
extension: .md # the extension of files that may be sitegen page parts
index-page-name: index # string representing the custom page name for the index
templates-dir: ./templates # directory to find templates
template-ext: .html  # the extension used for templates
css-dir: ./templates/css  # directory to find css files.
default-style: style.css  # name of the default style sheet.
generate-tags: true  # should sitegen generate a tags page
generate-categories: true  # should sitegen generate categories
-}

data RawSiteGenConfig = RawSiteGenConfig
    { _siteID :: !String
    , _source :: !FilePath
    , _outputDir :: !FilePath
    , _extension :: !String
    , _indexPageName :: !String
    , _templatesDir :: !FilePath
    , _templateExt :: !String
    , _cssDir :: !FilePath
    , _defaultStyle :: !String
    , _generateTags :: !Bool
    , _generateCategories :: !Bool
    } deriving (Show)


instance FromJSON RawSiteGenConfig where
    parseJSON (Object v) = RawSiteGenConfig
        <$> v .:  "site"                                       -- site: <site-identifier>
        <*> v .:? "source"              .!= "./src"            -- the directory (relative to the site.yaml) to start
        <*> v .:? "output-dir"          .!= "./html"           -- where to place output files
        <*> v .:? "extension"           .!= ".md"              -- the extension for source files
        <*> v .:? "index-page-name"     .!= "index"            -- The 'start' page for the site.
        <*> v .:? "templates-dir"       .!= "./templates"      -- directory to find templates
        <*> v .:? "template-ext"        .!= ".html"            -- the extension used for templates
        <*> v .:? "css-dir"             .!= "./templates/css"  -- directory to find css files.
        <*> v .:? "default-style"       .!= "style.css"        -- name of the default style sheet.
        <*> v .:? "generate-tags"       .!= False              -- should sitegen generate a tags page
        <*> v .:? "generate-categories" .!= False              -- should sitegen generate categories
    parseJSON _ = error "Can't parse SitegenConfig from YAML/JSON"


readConfig :: (MonadIO m, MonadThrow m) => FilePath -> m RawSiteGenConfig
readConfig fp = do
    res <- liftIO $ decodeFileEither fp
    case res of
        Left parseException -> throwM $ ConfigException $ show parseException
        Right conf -> return conf


data SiteGenConfig = SiteGenConfig
    { siteYaml :: !FilePath
    , siteID :: !String
    , source :: !FilePath
    , outputDir :: !FilePath
    , extension :: !String
    , indexPageName :: !String
    , templatesDir :: !FilePath
    , templateExt :: !String
    , cssDir :: !FilePath
    , defaultStyle :: !String
    , generateTags :: !Bool
    , generateCategories :: !Bool
    } deriving (Show)


getSiteGenConfig :: (MonadIO m, MonadThrow m)
                 => FilePath
                 -> m SiteGenConfig
getSiteGenConfig configFileName = do
    configPath <- liftIO $ makeAbsolute configFileName
    rawConfig <- readConfig configPath
    makeSiteGenConfigFromRaw configPath rawConfig


makeSiteGenConfigFromRaw :: (MonadIO m, MonadThrow m)
                          => FilePath
                          -> RawSiteGenConfig
                          -> m SiteGenConfig
makeSiteGenConfigFromRaw configPath rawConfig = do
    let root = takeDirectory configPath
    source_ <- liftIO $ resolvePathMaybe (_source rawConfig) root "source dir"
    outputDir_ <- liftIO $ resolvePathMaybe (_outputDir rawConfig) root "output dir"
    templatesDir_ <- liftIO $ resolvePathMaybe (_templatesDir rawConfig) root "templates dir"
    cssDir_ <- liftIO $ resolvePathMaybe (_cssDir rawConfig) root "css dir"
    if any isNothing [source_, outputDir_, templatesDir_, cssDir_]
      then throwM $ ConfigException "One or more directories didn't exist"
      else return $ SiteGenConfig
          { siteYaml=configPath
          , siteID=_siteID rawConfig
          , source= fromJust source_
          , outputDir=fromJust outputDir_
          , extension=_extension rawConfig
          , indexPageName=_indexPageName rawConfig
          , templatesDir=fromJust templatesDir_
          , templateExt=_templateExt rawConfig
          , cssDir=fromJust cssDir_
          , defaultStyle=_defaultStyle rawConfig
          , generateTags=_generateTags rawConfig
          , generateCategories=_generateCategories rawConfig
          }


resolvePathMaybe :: FilePath
                  -> FilePath
                  -> String
                  -> IO (Maybe FilePath)
resolvePathMaybe path root errorStr = do
    res <- try (resolvePath path root errorStr) :: IO (Either ConfigException FilePath)
    case res of
        Left ex -> do
             print ex
             return Nothing
        Right p -> return $ Just p


resolvePath :: (MonadIO m, MonadThrow m)
            => FilePath    -- the path to resolve
            -> FilePath    -- the root to perhaps append to it.
            -> String      -- an handy error string
            -> m FilePath  -- what to return
resolvePath path root errorStr = do
    resolvedPath <- if head path /= pathSeparator
                      then liftIO $ makeAbsolute (root </> path)
                      else pure path
    exists <- liftIO $ doesDirectoryExist resolvedPath
    if exists
      then return resolvedPath
      else throwM $ ConfigException ( "Path "
                                   ++ resolvedPath
                                   ++ " doesn't exist for: "
                                   ++ errorStr)
