{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Lib.Utils
    ( debugArgs
    , falseOr
    , fixRoute
    , flipSpaces
    , flipUnderscores
    , fmapToFst
    , fmapOnFst
    , isDebug
    , isMarkDownFile
    , isMarkDownStr
    , maybeM
    , maybeM'
    , printIfDoesntExist
    , returnFirstMaybeResultM
    , strToLower
    , validateDirExists
    , validateFileExists
    , validateWithTests
    ) where

import           Conduit
import           Control.Applicative (liftA2)
import           Data.Bifunctor      (first)
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


-- maybeM :: b -> (a -> m b) -> Maybe a -> m b
maybeM :: Monad m => b -> (a -> m b) -> Maybe a -> m b
maybeM def f mv = case mv of
    Nothing -> pure def
    Just v -> f v
maybeM' :: Monad m => b -> Maybe a -> (a -> m b) -> m b
maybeM' def mv f = case mv of
    Nothing -> pure def
    Just v -> f v


-- Helper that takes a Maybe value (which it doesn't care about), and if Nothing
-- returns m False, but if Just _ then returns the evaluation of the passed
-- function.
falseOr :: Monad m => Maybe a -> m Bool -> m Bool
falseOr v f = maybe (pure False) (const f) v


-- | sequentially apply a supplied list of monadic functions to the input and
-- return the result of the first one that returns a Just value.
returnFirstMaybeResultM :: Monad m => a -> [a -> m (Maybe b)] -> m (Maybe b)
returnFirstMaybeResultM link [] = pure Nothing
returnFirstMaybeResultM link (f:fs) = f link >>= \case
    Nothing -> returnFirstMaybeResultM link fs
    res -> pure res


-- | fmap the function onto a functor and return a functor with a tuple of the
-- result and the original value.
fmapToFst :: Functor f => (a -> b) -> f a -> f (b, a)
fmapToFst f = fmap (\x -> (f x, x))


-- | Apply a function to the first element of a tuple in a functor.
fmapOnFst :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
fmapOnFst f = fmap (first f)
