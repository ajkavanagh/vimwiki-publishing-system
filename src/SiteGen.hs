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
    {-( someFunc-}
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where

import           Data.Function             ((&))

import           System.Exit               (ExitCode (..), exitWith)

-- for optparse
import           Data.Semigroup            ((<>))
import           Options.Applicative
import           Options.Applicative.Types (ReadM, readerAsk)

-- monad related stuff
import           Control.Monad             (foldM, liftM2, unless, when)

-- Polysemy
import           Colog.Core                (logStringStdout)
import           Colog.Polysemy            (runLogAction)
import           Polysemy
import           Polysemy.Error

-- Local Libraries
import           Lib                       (isDebug, isMarkDownFile,
                                            isMarkDownStr, printIfDoesntExist,
                                            strToLower, validateFileExists,
                                            validateWithTests)
import           SiteGenConfig             (ConfigException, SiteGenConfig,
                                            getSiteGenConfig)

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
    <*> many (argument str
        ( metavar "EXTRA"
       <> help "Extra argments"))


opts :: ParserInfo SitegenArgs
opts = info
    ( sitegenArgsOptions <**> helper )
    ( fullDesc
    <> progDesc "Transform a Pandoc markdown file into an HTML file"
    <> header "sitegen - convert vimwiki/markdown to html as a static site"
    <> noIntersperse )


validateSitegenArgs :: SitegenArgs -> IO Bool
validateSitegenArgs args = validateWithTests args tests
  where
      tests = [ validateFileExists siteConfigArg "Config file " ]


{- This is where the sitegen program proper starts. -}

-- we've now got some validated args; now we need to use those args to create
-- the full read-only config for the site from the site.yaml.  Then we use that
-- to create the sitegen
runSiteGen :: SitegenArgs -> IO ()
runSiteGen args = do
    res <- runGetSiteGenConfig args
    case res of
        Right sgc -> do
            print sgc
            return ()
        Left ex -> do
            print ex
            exitWith (ExitFailure 1)


runGetSiteGenConfig :: SitegenArgs -> IO (Either ConfigException SiteGenConfig)
runGetSiteGenConfig args = do
    --  Resolve the Sem monads with each interpret function to get the sitegen
    let fp = siteConfigArg args
        draft = draftsArg args
    getSiteGenConfig fp draft               -- [Log String, Embed IO, Error ConfigException]
        & runLogAction @IO logStringStdout  -- [Embed IO, Error ConfigException]
        & errorToIOFinal @ConfigException   -- [Embed IO]
        & embedToFinal @IO
        & runFinal
