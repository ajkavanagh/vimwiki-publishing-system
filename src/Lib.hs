{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Lib
    {-( someFunc-}
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where

import Conduit
import System.FilePath
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.List as L
import qualified Text.Pandoc as TP
import qualified Text.Pandoc.Error as TPE
import qualified Data.Text.IO as TIO
import Control.Applicative (liftA2)

-- for system environment, etc.
import System.Environment (getProgName, getArgs)

-- for optparse
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (ReadM, readerAsk)

-- for RIO
import Control.Monad.IO.Class (MonadIO, liftIO)

someFunc :: IO ()
someFunc = runResourceT $ runConduit
     $ sourceDirectoryDeep False "."
    .| filterC isMarkDownFile
    .| mapM_C (liftIO . putStrLn)

isMarkDownStr :: String -> Bool
isMarkDownStr = liftA2 (||) (== "md") (== "markdown") . strToLower

isMarkDownExt :: String -> Bool
isMarkDownExt = isMarkDownStr . safeTail

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

isMarkDownFile :: FilePath -> Bool
isMarkDownFile = isMarkDownExt . takeExtension

doThing :: Show a => a -> IO a
doThing x = do
    putStrLn $ "Side effect with " ++ show x
    return x

doOtherThing :: Show a => a -> IO ()
doOtherThing x = putStrLn $ "Sidy with: " ++ show x

someOtherFunc :: IO ()
someOtherFunc = do
    putStrLn "someOtherFunc"
    runConduit
         $ yieldMany [1..]
        .| takeC 10
        .| mapC (*2)
        .| takeWhileC (<18)
        .| mapMC doThing
        .| mapMC (\x -> do {
            putStrLn $ "Another side effect with " ++ show x;
            return $ x * 2; } :: IO Integer)
        .| iterMC doOtherThing
        .| iterMC print
        .| mapM_C print


testMain :: IO ()
testMain = undefined


-- from https://www.fpcomplete.com/blog/2017/07/the-rio-monad
-- A way to do a reader wrapped around an IO
newtype RIO env a = RIO (env -> IO a)
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO f) = liftIO (f env)

-- check the arguments, returning a Either Text () as a result - this is a pure
-- function so that we don't have to run it in IO
{-validateVWSingleFileArgs :: VimwikiSingleFileCliArgs -> IO (Either T.Text ())-}
{-validateVWSingleFileArgs vwArgs = do-}
    {-validateExtension (extension vmArgs)-}
    {-validateInputFileExists (inputFile vmArgs)-}
    {-validateSyntax (syntax vmArgs)-}

strToLower :: String -> String
strToLower = map C.toLower
