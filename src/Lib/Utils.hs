{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib.Utils
    ( isMarkDownStr
    , isMarkDownFile
    , debugArgs
    , isDebug
    , strToLower
    , validateWithTests
    , validateFileExists
    , validateDirExists
    , printIfDoesntExist
    , flipUnderscores
    , flipSpaces
    , fixRoute
    ) where

import           Conduit
import           Control.Applicative (liftA2)
import qualified Data.Char           as C
import           System.FilePath

-- for system environment, etc.
import           System.Directory    (doesDirectoryExist, doesFileExist)
import           System.Environment  (getArgs, getEnvironment)

-- Monad helpers
import           Control.Monad       (foldM, liftM2)


isMarkDownStr :: String -> Bool
isMarkDownStr = liftA2 (||) (== "md") (== "markdown") . strToLower

isMarkDownExt :: String -> Bool
isMarkDownExt = isMarkDownStr . safeTail

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

isMarkDownFile :: FilePath -> Bool
isMarkDownFile = isMarkDownExt . takeExtension


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
    pure $ not (null debug) && case head debug of
        (_,"") -> False
        _      -> True


strToLower :: String -> String
strToLower = map C.toLower


flipUnderscores :: String -> String
flipUnderscores = map go
  where
      go '_' = '-'
      go x   = x

flipSpaces :: String -> String
flipSpaces = map go
  where
      go ' ' = '-'
      go x   = x


ensureStartSlash :: String -> String
ensureStartSlash "" = "/"
ensureStartSlash ('.':xs) = ensureStartSlash xs
ensureStartSlash xs@('/':_) = xs
ensureStartSlash xs = '/':xs


fixRoute :: String -> String
fixRoute = ensureStartSlash . flipSpaces . flipUnderscores . strToLower

validateWithTests :: a -> [a -> IO Bool] -> IO Bool
validateWithTests a = foldM ander True
  where
      mAnd = liftM2 (&&)
      ander acc test = pure acc `mAnd` test a


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
    pure exists


printIfDoesntExist :: Bool -> String -> String -> IO ()
printIfDoesntExist True _ _   = pure ()
printIfDoesntExist False path prefix = putStrLn $ prefix ++ path ++ " doesn't exist!"
