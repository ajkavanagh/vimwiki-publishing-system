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
import qualified Data.HashMap.Strict       as HashMap
import           Data.List                 (intercalate)

import           System.Exit               (ExitCode (..), exitWith)

-- for optparse
import           Data.Semigroup            ((<>))
import           Options.Applicative
import           Options.Applicative.Types (ReadM, readerAsk)

-- monad related stuff
import           Control.Monad             (foldM, forM_, liftM2, unless, when)

-- Polysemy
import           Colog.Core                (logStringStderr)
import           Colog.Polysemy            (Log, runLogAction)
import qualified Colog.Polysemy            as CP
import           Polysemy
import           Polysemy.Error            (Error, errorToIOFinal, mapError)
import qualified Polysemy.Error            as PE
import           Polysemy.Reader           (runReader)
import           Polysemy.State            (runState)

-- Application Effects
import           Effect.ByteStringStore    (BSHMStore, bsStoreAsHash)
import           Effect.File               (File, FileException, fileToIO)
import           Effect.Ginger             (GingerException (..))

-- Local Libraries
import           Lib.RenderUtils           (renderSourceContext)
import           Lib.Errors                (SiteGenError, mapSiteGenError)
import qualified Lib.Files                 as F
import qualified Lib.Header                as H
import qualified Lib.RouteUtils            as RU
import           Lib.SiteGenConfig         (ConfigException, SiteGenConfig (..))
import qualified Lib.SiteGenConfig         as SGC
import           Lib.SiteGenState          (SiteGenReader, SiteGenState,
                                            emptySiteGenState,
                                            makeSiteGenReader
                                            , nextSCToRender
                                            , addToRenderList )
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
        & mapError @ConfigException mapSiteGenError
        & mapError @FileException mapSiteGenError
        & mapError @GingerException mapSiteGenError
        & mapError @FileException mapSiteGenError
        & errorToIOFinal @SiteGenError
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


-- we've now got some validated args; now we need to use those args to create
-- the full read-only config for the site from the site.yaml.  Then we use that
-- to create the sitegen
runSiteGenSem
    :: Members '[ Log String
                , File
                , Error FileException
                , Error ConfigException
                , Error GingerException
                , Error SiteGenError
                ] r
    => SitegenArgs -> Sem r ()
runSiteGenSem args = do
    let fp = siteConfigArg args
        draft = draftsArg args
    sgc <- SGC.getSiteGenConfig fp draft
    CP.log @String "The site gen config is"
    CP.log @String $ show sgc
    let sourceDir = SGC.sgcSource sgc
    let ext = SGC.sgcExtension sgc
    CP.log @String $ "Looking in " ++ show sourceDir
    CP.log @String $ "Extension is " ++ show ext
    spcs <- runReader sgc $ F.filePathToSourcePageContexts sourceDir ext
    CP.log @String $ "SPCs are:\n" ++ intercalate "\n" (map show spcs)
    let files = map H.spcRelFilePath spcs
    CP.log @String $ "Files are: " ++ intercalate ", " files
    CP.log @String $ "Routes are: " ++ intercalate ", " (map (show . H.spcRoute) spcs)
    let dr = RU.checkDuplicateRoutesSPC spcs
    CP.log @String $ "Duplicate routes: " ++ intercalate ", " (map show dr)
    let mr = RU.findMissingIndexRoutesSPC spcs
    CP.log @String $ "Missing routes: Len (" ++ show (length mr) ++ ") = " ++ intercalate ", " mr
    let spcs' = RU.ensureIndexRoutesIn spcs
        scs = RU.addVPCIndexPages spcs'
    CP.log @String $ "Final route set: " ++ intercalate ", " (map H.scRoute scs)
    {-CP.log @String $ "Final SourceContext set:\n" ++ intercalate "\n" (map show scs)-}
    -- Create the SiteGenState and Reader
    let sgr = makeSiteGenReader scs

    let scs' = if not (null (extraArgs args))
             then let fp' = head $ extraArgs args
                   in map H.SPC $ filter ((==fp').H.spcRelFilePath) spcs
             else scs
    -- now just call the rendering function to render all of these files
    -- Note that they are put into the renderList with addToRenderList and taken
    -- out with nextSCToRender.  This means that rendering can add to the list
    -- (e.g. pagination or other dynamic tasks that might generate additional
    -- pages)
    runReader @SiteGenConfig sgc
        $ runReader @SiteGenReader sgr
        $ runState @BSHMStore HashMap.empty
        $ bsStoreAsHash
        $ runState @SiteGenState emptySiteGenState
        $ do
            addToRenderList scs'
            let go = do mSc <- nextSCToRender
                        case mSc of
                            Just sc -> renderSourceContext sc >> go   -- render the file and loop
                            Nothing -> pure ()
            go
            when (sgcCopyStaticFiles sgc) F.copyStaticFiles

    CP.log @String $ "Extra: " ++ show (extraArgs args)

    pure ()
