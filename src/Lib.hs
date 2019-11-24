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
import System.Directory (doesFileExist, doesDirectoryExist)
import System.Environment (getProgName, getArgs, getEnvironment)

-- for optparse
import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (ReadM, readerAsk)

-- Monad helpers
import Control.Monad (foldM, liftM2)

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


debugArgs :: IO ()
debugArgs = do
    args <- getArgs
    putStrLn "The args passed to the app on the command line were:"
    putStrLn $ unwords args
    putStrLn "------"


isDebug :: IO Bool
isDebug = do
    envVars <- getEnvironment
    let debug = filter ((=="DEBUG").fst) envVars
    return $ not (null debug) && case head debug of
        (_,"") -> False
        _      -> True


-- from https://www.fpcomplete.com/blog/2017/07/the-rio-monad
-- A way to do a reader wrapped around an IO
newtype RIO env a = RIO (env -> IO a)
runRIO :: MonadIO m => env -> RIO env a -> m a
runRIO env (RIO f) = liftIO (f env)


strToLower :: String -> String
strToLower = map C.toLower


validateWithTests :: a -> [a -> IO Bool] -> IO Bool
validateWithTests a = foldM ander True
  where
      mAnd = liftM2 (&&)
      ander acc test = return acc `mAnd` test a


validateFileExists :: (a -> String)
                   -> String
                   -> a
                   -> IO Bool
validateFileExists = validateThingExists doesFileExist


validateDirExists :: (a -> FilePath)
                  -> String
                  -> a
                  -> IO Bool
validateDirExists = validateThingExists doesDirectoryExist


validateThingExists :: (FilePath -> IO Bool)
                    -> (a -> FilePath)
                    -> String
                    -> a
                    -> IO Bool
validateThingExists test f errorStr args = do
    let path = f args
    exists <- test path
    printIfDoesntExist exists path errorStr
    return exists


printIfDoesntExist :: Bool -> String -> String -> IO ()
printIfDoesntExist True _ _   = return ()
printIfDoesntExist False path prefix = putStrLn $ prefix ++ path ++ " doesn't exist!"
