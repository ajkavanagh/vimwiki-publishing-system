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


module SiteGen
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where

import           Data.Function             ((&))
import           Data.List                 (intercalate)

import           System.Exit               (ExitCode (..), exitWith)

-- for optparse
import           Data.Semigroup            ((<>))
import           Options.Applicative
import           Options.Applicative.Types (ReadM, readerAsk)

-- monad related stuff
import           Control.Monad             (foldM, liftM2, unless, when)

-- Polysemy
import           Colog.Core                (logStringStderr)
import           Colog.Polysemy            (Log, runLogAction)
import qualified Colog.Polysemy            as CP
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader           (runReader)

-- Application Effects
import           Effect.File               (File, FileException, fileToIO)

-- Local Libraries
import qualified Lib.Files                 as F
import qualified Lib.Header                as H
import           Lib.Utils                 (isDebug, isMarkDownFile,
                                            isMarkDownStr, printIfDoesntExist,
                                            strToLower, validateFileExists,
                                            validateWithTests)
import           Lib.SiteGenConfig         (ConfigException, SiteGenConfig)
import qualified Lib.SiteGenConfig         as SGC

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
        & runLogAction @IO logStringStderr
        & fileToIO
        & errorToIOFinal @ConfigException
        & errorToIOFinal @FileException
        & embedToFinal @IO
        & runFinal
    case res of
        Right _ -> do
            return ()
        Left ex -> do
            print ex
            exitWith (ExitFailure 1)


-- we've now got some validated args; now we need to use those args to create
-- the full read-only config for the site from the site.yaml.  Then we use that
-- to create the sitegen
runSiteGenSem
    :: Members '[ Log String
                , File
                , Error FileException
                , Error ConfigException ] r
    => SitegenArgs -> Sem r ()
runSiteGenSem args = do
    let fp = siteConfigArg args
        draft = draftsArg args
    sgc <- SGC.getSiteGenConfig fp draft
    {-CP.log @String "The site gen config is"-}
    {-CP.log @String $ show sgc-}
    let sourceDir = SGC.sgcSource sgc
    let ext = SGC.sgcExtension sgc
    CP.log @String $ "Looking in " ++ (show sourceDir)
    CP.log @String $ "Extension is " ++ (show ext)
    phs <- runReader sgc $ F.filePathToSourcePageHeaders sourceDir ext
    let files = map H.phFileName phs
    CP.log @String $ "Files are " ++ (intercalate "\n" files)
    pure ()
