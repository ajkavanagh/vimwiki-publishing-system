{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module SiteGenConfig where

import           System.FilePath  (FilePath, pathSeparator, takeDirectory,
                                   (</>))

import           Data.List        (intercalate)
import           Data.Maybe       (fromJust, isNothing)
import qualified Data.Yaml        as Y
import           Data.Yaml        ((.:), (.:?), (.!=))

-- For Polysemy logging of things going on.
import           Colog.Polysemy   (Log)
import qualified Colog.Polysemy   as CP
import           Polysemy         (Embed, Members, Sem, embed)
import           Polysemy.Error   (Error, throw)

import           Effect.File      (File, FileException)
import qualified Effect.File      as EF


-- | define the biggest file we are willing to process
maxFileToProcessSize :: Int
maxFileToProcessSize = 20 * 1024


data ConfigException = ConfigException String
                     | ConfigExceptions [ConfigException]

instance Show ConfigException where
    show ex = "Config Loading failed due to: " ++ ss
      where
          ss = case ex of
              (ConfigException s)   -> s
              (ConfigExceptions xs) -> intercalate ", " $ map show xs

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
    { _siteID             :: !String
    , _source             :: !FilePath
    , _outputDir          :: !FilePath
    , _extension          :: !String
    , _indexPageName      :: !String
    , _templatesDir       :: !FilePath
    , _templateExt        :: !String
    , _cssDir             :: !FilePath
    , _defaultStyle       :: !String
    , _generateTags       :: !Bool
    , _generateCategories :: !Bool
    , _publishDrafts      :: !Bool
    } deriving (Show)


instance Y.FromJSON RawSiteGenConfig where
    parseJSON (Y.Object v) = RawSiteGenConfig
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
        <*> v .:? "publish-drafts"      .!= False              -- should we publish drafs?
    parseJSON _ = error "Can't parse SitegenConfig from YAML/JSON"


readConfig
    :: Members '[ Log String
                , File
                , Error FileException
                , Error ConfigException
                ] r
       => FilePath
       -> Sem r RawSiteGenConfig
readConfig fp = do
    bs <- EF.readFile fp Nothing Nothing
    case Y.decodeEither' bs of
        Left parseException -> throw $ ConfigException $ show parseException
        Right conf          -> pure conf


data SiteGenConfig = SiteGenConfig
    { sgcSiteYaml           :: !FilePath
    , sgcSiteID             :: !String
    , sgcSource             :: !FilePath
    , sgcOutputDir          :: !FilePath
    , sgcExtension          :: !String
    , sgcIndexPageName      :: !String
    , sgcTemplatesDir       :: !FilePath
    , sgcTemplateExt        :: !String
    , sgcCssDir             :: !FilePath
    , sgcDefaultStyle       :: !String
    , sgcGenerateTags       :: !Bool
    , sgcGenerateCategories :: !Bool
    , sgcPublishDrafts      :: !Bool
    } deriving (Show)


getSiteGenConfig
    :: Members '[ Log String
                , File
                , Error FileException
                , Error ConfigException
                ] r
       => FilePath
       -> Bool
       -> Sem r SiteGenConfig
getSiteGenConfig configFileName forceDrafts = do
    configPath <- EF.makeAbsolute configFileName
    rawConfig <- readConfig configPath
    makeSiteGenConfigFromRaw configPath rawConfig forceDrafts


makeSiteGenConfigFromRaw
    :: Members '[ Log String
                , File
                , Error FileException
                , Error ConfigException
                ] r
       => FilePath
       -> RawSiteGenConfig
       -> Bool
       -> Sem r SiteGenConfig
makeSiteGenConfigFromRaw configPath rawConfig forceDrafts = do
    let root = takeDirectory configPath
    source_ <- resolvePath (_source rawConfig) root "source dir"
    outputDir_ <- resolvePath (_outputDir rawConfig) root "output dir"
    templatesDir_ <- resolvePath (_templatesDir rawConfig) root "templates dir"
    cssDir_ <- resolvePath (_cssDir rawConfig) root "css dir"
    if any isNothing [source_, outputDir_, templatesDir_, cssDir_]
      then throw $ ConfigException "One or more directories didn't exist"
      else pure $ SiteGenConfig
          { sgcSiteYaml=configPath
          , sgcSiteID=_siteID rawConfig
          , sgcSource=fromJust source_
          , sgcOutputDir=fromJust outputDir_
          , sgcExtension=_extension rawConfig
          , sgcIndexPageName=_indexPageName rawConfig
          , sgcTemplatesDir=fromJust templatesDir_
          , sgcTemplateExt=_templateExt rawConfig
          , sgcCssDir=fromJust cssDir_
          , sgcDefaultStyle=_defaultStyle rawConfig
          , sgcGenerateTags=_generateTags rawConfig
          , sgcGenerateCategories=_generateCategories rawConfig
          , sgcPublishDrafts=_publishDrafts rawConfig || forceDrafts
          }


resolvePath
    :: Members '[ Log String
                , File
                , Error FileException
                ] r
    => FilePath        -- The path to resolve
    -> FilePath        -- the root to perhaps append to it.
    -> String          -- A handy error string to log with (maybe)
    -> Sem r (Maybe FilePath)  -- what to return
resolvePath "" _ errorStr = do
    CP.log @String $ "Path  is empty for: " ++ errorStr
    pure Nothing
resolvePath path root errorStr = do
    resolvedPath <- if head path /= pathSeparator
                      then EF.makeAbsolute (root </> path)
                      else pure path
    exists <- EF.doesDirectoryExist resolvedPath
    if exists
      then pure $ Just resolvedPath
      else do
          CP.log @String $ "Path " ++ resolvedPath ++ " doesn't exist for: " ++ errorStr
          pure Nothing
