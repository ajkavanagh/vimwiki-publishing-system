{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# LANGUAGE PatternSynonyms      #-}

module SiteGen
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where

import           TextShow

import           Data.Function             ((&))
import qualified Data.HashMap.Strict       as HashMap
import           Data.List                 (intercalate)
import qualified Data.Text                 as T

import           System.Exit               (ExitCode (..), exitWith)
import           System.FilePath           (makeRelative)

-- for optparse
import           Data.Semigroup            ((<>))
import           Options.Applicative
import           Options.Applicative.Types (ReadM, readerAsk)

-- monad related stuff
import           Control.Monad             (foldM, forM_, liftM2, unless, when)

-- Polysemy
import           Colog.Core                (logStringStderr)
import           Colog.Core.Severity       (pattern D, pattern E, pattern I,
                                            Severity, pattern W)
import           Colog.Polysemy            (Log, runLogAction)
import qualified Colog.Polysemy            as CP
import           Polysemy
import           Polysemy.Error            (Error, errorToIOFinal, mapError)
import qualified Polysemy.Error            as PE
import           Polysemy.Reader           (runReader)
import           Polysemy.State            (evalState, runState)

import           Text.Ginger               (SourcePos, Template)
import           Text.Pandoc               (Pandoc)

-- Application Effects
import           Effect.Cache              (Cache, CacheStore, cacheInHashWith,
                                            emptyCache)
import           Effect.File               (File, FileException, fileToIO, getCurrentDirectory)
import           Effect.Locale             (Locale, LocaleException, localeToIO)
import           Effect.Logging            (LoggingMessage, logActionLevel)
import           Effect.Print              (Print, printToIO)
import qualified Effect.Print              as P

-- Local Libraries
import           Types.Errors              (SiteGenError (..), mapSiteGenError)
import           Types.Ginger              (GingerException)
import           Types.Header              (SourceMetadata (..))

import qualified Lib.Files                 as F
import qualified Lib.Header                as H
import           Lib.RenderUtils           (renderSourceMetadata)
import qualified Lib.RouteUtils            as RU
import           Lib.SiteGenConfig         (ConfigException, SiteGenConfig (..))
import qualified Lib.SiteGenConfig         as SGC
import           Lib.SiteGenState          (SiteGenReader, SiteGenState,
                                            addToRenderList, emptySiteGenState,
                                            lengthRenderList, makeSiteGenReader,
                                            nextSMToRender)
import           Lib.SpecialPages.Category (resolveCategoriesPage)
import           Lib.SpecialPages.Four04   (resolve404page)
import           Lib.SpecialPages.Tag      (resolveTagsPage)
import           Lib.Utils                 (isDebug, isMarkDownFile,
                                            isMarkDownStr, printIfDoesntExist,
                                            strToLower, validateFileExists,
                                            validateWithTests)


sitegenProgram :: IO ()
sitegenProgram = sitegenCli =<< execParser opts


sitegenCli :: SitegenArgs -> IO ()
sitegenCli args = do
    {-debug <- isDebug-}
    {-when debug $ do-}
        {-putStrLn "-- Debug Trace On --"-}
        {-debugParams args-}
    argsOk <- validateSitegenArgs args
    when argsOk $ runSiteGen args


data SitegenArgs = SitegenArgs
    { siteConfigArg :: !String
    , draftsArg     :: !Bool
    , cleanArg      :: !Bool
    , extraArgs     :: ![String]
    }
    deriving Show


-- debug helper to create a SitegenArgs from some values
makeSitegenArgs :: String -> Bool -> Bool -> FilePath -> SitegenArgs
makeSitegenArgs s f1 f2 fp =
    let extra = [fp | not (null fp)]
     in SitegenArgs { siteConfigArg=s
                    , draftsArg=f1
                    , cleanArg=f2
                    , extraArgs=extra
                    }


sitegenArgsOptions :: Parser SitegenArgs
sitegenArgsOptions = SitegenArgs
    <$> strOption
        ( long "config"
       <> short 'c'
       <> metavar "SITE_CONFIG"
       <> help "The config file for the site" )
    <*> switch
        ( long "drafts"
       <> short 'd'
       <> help "Use to include draft posts in site" )
    <*> switch
        ( long "clean"
       <> short 'x'
       <> help "Clean out target files (.html) that don't aren't linked to a source file" )
    <*> many (argument str
        ( metavar "EXTRA"
       <> help "Extra argments"))


opts :: ParserInfo SitegenArgs
opts = info
    ( sitegenArgsOptions <**> helper )
    ( fullDesc
    <> progDesc "Transform a Pandoc markdown file into an HTML file"
    <> header "sitegen - convert a sitegen enabled vimwiki/markdown to html as a static site"
    <> noIntersperse )


validateSitegenArgs :: SitegenArgs -> IO Bool
validateSitegenArgs args = validateWithTests args tests
  where
      tests = [ validateFileExists siteConfigArg "Config file " ]


{- This is where the sitegen program proper starts. -}

-- | Run the Sem r @runSiteGenSem@ function that runs the entire program in the
-- Sem r monad.  This interprets the logs to stderr and converts exceptions into
-- a Either, prints it and exits with a 1.
-- Note: the only error so far are @ConfigException@; we'll have to extend that
-- as time goes on.
runSiteGen :: SitegenArgs -> IO ()
runSiteGen args = do
    res <- runSiteGenSem args
        & fileToIO
        & localeToIO
        & mapError @ConfigException mapSiteGenError
        & mapError @FileException mapSiteGenError
        & mapError @GingerException mapSiteGenError
        & mapError @FileException mapSiteGenError
        & mapError @LocaleException mapSiteGenError
        & printToIO @IO
        & errorToIOFinal @SiteGenError
        & runLogAction @IO (logActionLevel I)
        & runLogAction @IO logStringStderr
        & embedToFinal @IO
        & runFinal @IO
    case res of
        Right _ ->
            return ()
        Left ex -> do
            print ex
            exitWith (ExitFailure 1)


runSiteGenHelper s fp = do
    let args = makeSitegenArgs s True True fp
    runSiteGen args


printInfoHeader
    :: ( Member Print r
       , Member File r
       )
    => SGC.SiteGenConfig -> Sem r ()
printInfoHeader sgc = do
    P.putText "\n------------------------------------------------"
    P.putText "sitegen: Static site generator for vimwiki links"
    P.putText "------------------------------------------------\n"
    pwd <- getCurrentDirectory
    let root = sgcRoot sgc
        root' = makeRelative pwd root
        pRoot = if length root' < length root then "./" <> root' else root
    P.putText $ "Root directory is   : " <> T.pack pRoot
    P.putText $ "Source Directory is : " <> T.pack (SGC.dirForPrint sgcSource sgc)
    P.putText $ "Output Directory is : " <> T.pack (SGC.dirForPrint sgcOutputDir sgc)
    P.putText $ "Statics Directory is: " <> T.pack (SGC.dirForPrint sgcStaticDir sgc)
    P.putText $ "Extension is        : " <> T.pack (sgcExtension sgc)
    P.putText ""


-- we've now got some validated args; now we need to use those args to create
-- the full read-only config for the site from the site.yaml.  Then we use that
-- to create the sitegen
runSiteGenSem
    :: Members '[ Log LoggingMessage
                , File
                , Locale
                , Print
                , Error FileException
                , Error ConfigException
                , Error GingerException
                , Error SiteGenError
                ] r
    => SitegenArgs -> Sem r ()
runSiteGenSem args = do
    let fp = siteConfigArg args
        draft = draftsArg args
        clean = cleanArg args
    sgc <- SGC.getSiteGenConfig fp draft
    printInfoHeader sgc
    let sourceDir = SGC.sgcSource sgc
    let ext = SGC.sgcExtension sgc
    sms <- runReader sgc $ F.filePathToSourceMetadataItems sourceDir ext
    let dr = RU.checkDuplicateRoutes sms
    unless (null dr) $ PE.throw @SiteGenError $ OtherError
        $ T.pack $ "Duplicate routes: " ++ intercalate ", " (map show dr)
    let mr = RU.findMissingIndexRoutes sms
    let sms' = RU.ensureIndexRoutesIn sms
        scs = RU.addVSMIndexPages sms'
    let sgr = makeSiteGenReader scs

    -- TODO: filtering whilst in debug; pass in from the command line.
    let scs' = if not (null (extraArgs args))
             then let fp' = head $ extraArgs args
                   in filter ((== Just fp') . smRelFilePath) sms
             else scs
    -- now just call the rendering function to render all of these files
    -- Note that they are put into the renderList with addToRenderList and taken
    -- out with nextSMToRender.  This means that rendering can add to the list
    -- (e.g. pagination or other dynamic tasks that might generate additional
    -- pages)
    runReader @SiteGenConfig sgc
        $ runReader @SiteGenReader sgr
        $ cacheInHashWith (emptyCache @Pandoc)
        $ cacheInHashWith (emptyCache @(Template SourcePos))
        $ runState @SiteGenState emptySiteGenState
        $ do
            addToRenderList scs'
            resolve404page
            when (sgcGenerateCategories sgc) resolveCategoriesPage
            when (sgcGenerateTags sgc) resolveTagsPage
            numToRender <- lengthRenderList
            P.putText $ T.pack $ "Rendering " ++ show numToRender ++ " main files:\n"
            let go = do mSm <- nextSMToRender
                        case mSm of
                            Just sm -> renderSourceMetadata sm >> go   -- render the file and loop
                            Nothing -> pure ()
            go
            when (sgcCopyStaticFiles sgc) F.copyStaticFiles

            -- finally, delete any files that shouldn't be in the output
            -- directories now.
            when clean $ do
                P.putText "Cleaning any left over files (if any)"
                F.deleteNonMemoedFiles

    P.putText "Done."
