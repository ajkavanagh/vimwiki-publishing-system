{-# LANGUAGE OverloadedStrings #-}

module Lib.FileWatcher where

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import Control.Concurrent (threadDelay, myThreadId)
import Control.Exception (finally)
import Control.Monad (forever, forM)
import Data.List (isSuffixOf)
import qualified System.Directory   as SD
import qualified System.FSNotify as FS
import System.FilePath.Posix (makeRelative, (</>))

import Lib.BChan (BChan, writeBChan, newBChan, readBChan)
import Lib.Utils (strToLower)

{-
   Watch files in another thread.  The thread for file watching is not the main
   thread, and so it needs to communicate with the main thread via an MVar.
   This allows the main thread update the in-memory structures so that it can
   serve files.

   However, we have to hold the thread in a state var, and that most likely is a
   MonadState. We only need to know which directory to watch, and that can be
   passed as a paramter.  We could just return the thread as a value from the
   function.  It's by far the easiest thing to do, and means that this can be a
   self-contained function.
-}


-- A type to represent file changes
data FileChangeNotification =
    FileChangedInTree String
  | FileChanged String
    deriving (Eq, Show)


-- | watch a tree of files from a directory.  This is used to watch things in
-- the directory. e.g. filterP can be (hasSuffixOf [".md", ".markdown"]) 
startDirWatcher
    :: BChan FileChangeNotification 
    -> String
    -> (FilePath -> Bool)
    -> IO (Async ())
startDirWatcher bQueue dir filterP =
    startWatcherP bQueue
                  dir
                  FS.watchTree
                  filterP
                  FileChangedInTree


-- | watch a set of files in a single directory that is named.
startDirFileWatcher
    :: BChan FileChangeNotification 
    -> String
    -> [String]
    -> IO (Async ())
startDirFileWatcher bQueue dir files = do
    cDir <- SD.canonicalizePath dir
    cFiles <- forM files $ \file -> SD.canonicalizePath (cDir </> file)
    startWatcherP bQueue
                  dir
                  FS.watchDir
                  (`elem` cFiles)
                  FileChanged


startWatcherP
    :: BChan a
    -> String
    ->   (FS.WatchManager
       -> FilePath
       -> FS.ActionPredicate
       -> FS.Action
       -> IO FS.StopListening)
    -> (FilePath -> Bool)
    -> (String -> a)
    -> IO (Async ())
startWatcherP bQueue dir watchFunc filterP notifyF =
    Async.async $ FS.withManager $ \manager -> do
        watchFunc manager dir (shouldActOnFileChangeF filterP) (handleFileChangeF bQueue notifyF)
        forever (threadDelay maxBound) `finally` FS.stopManager manager


-- give the thread returned from startFileWatcher, we can stop it on demand
stopWatcher :: Async () -> IO ()
stopWatcher thread = Async.uninterruptibleCancel thread


-- decide whether to act on the file change using a predicate that makes the
-- decision.
-- Note ActionPredicate == (Event -> Bool)
shouldActOnFileChangeF :: (FilePath -> Bool) -> FS.ActionPredicate
shouldActOnFileChangeF filterP event =
    let path = getEventFilePath event
     in filterP path


-- | handy helper to see if the passed string has a suffix that is listed in the
-- first parameter.  Note that it lower-cases the filename to get the match.
hasSuffixOf :: [String] -> String -> Bool
hasSuffixOf [] _ = False
hasSuffixOf (x:xs) path = (((x `isSuffixOf`) . strToLower) path) || hasSuffixOf xs path


-- Handle the File Change and report the action using a constructor from String
-- to type a into the queue that is a BChan a.
-- Note Action == (Event -> IO ())
handleFileChangeF
    :: BChan a
    -> (String -> a)
    -> FS.Action
handleFileChangeF bQueue reportF event = do
    let filePath = getEventFilePath event
    writeBChan bQueue (reportF filePath)


getEventFilePath :: FS.Event -> FilePath
getEventFilePath event = case event of
    FS.Added filePath _ _ -> filePath
    FS.Modified filePath _ _ -> filePath
    FS.Removed filePath _ _ -> filePath
    FS.Unknown filePath _ _ -> filePath


-- watch a directory, and print the file that's been changed.  Blocks waiting
-- for the file-watcher to do it's thing.
test :: String -> IO ()
test dir = do
    bchan <- newBChan 2
    fw <- startDirWatcher bchan dir (const True)
    let loop = do
        file <- readBChan bchan
        putStrLn (show file)
        loop
    loop

