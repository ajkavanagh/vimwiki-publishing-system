{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}


module SiteGen
    {-( someFunc-}
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where

-- for optparse
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (ReadM, readerAsk)

-- monad related stuff
import Control.Monad (when, unless, foldM, liftM2)
import Control.Exception.Safe (Exception, MonadThrow, SomeException, throwM, catch, try)

-- Local Libraries
import SiteGenConfig ( SiteGenConfig
                     , getSiteGenConfig
                     )
import Lib ( isMarkDownStr
           , isMarkDownFile
           , strToLower
           , validateWithTests
           , printIfDoesntExist
           , validateFileExists
           , isDebug
           )

sitegenProgram :: IO ()
sitegenProgram = sitegenCli =<< execParser opts


sitegenCli :: SitegenArgs -> IO ()
sitegenCli args = do
    {-debug <- isDebug-}
    {-when debug $ do-}
        {-putStrLn "-- Debug Trace On --"-}
        {-debugParams args-}
    argsOk <- validateSitegenArgs args
    when argsOk $ runSitegen args


data SitegenArgs = SitegenArgs
    { siteConfigArg :: !String
    , extraArgs :: ![String]
    }
    deriving Show


sitegenArgsOptions :: Parser SitegenArgs
sitegenArgsOptions = SitegenArgs
    <$> strOption
        ( long "config"
       <> short 'c'
       <> metavar "SITE_CONFIG"
       <> help "The config file for the site" )
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
runSitegen :: SitegenArgs -> IO ()
runSitegen args = do
    res <- try (getSiteGenConfig (siteConfigArg args)) :: IO (Either SomeException SiteGenConfig)
    case res of
        Left ex -> putStrLn $ "Disaster occurred ... " ++ show ex
        Right sgc -> print sgc
