{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Effect.File
      where

import           Prelude            hiding (readFile)

import           TextShow

import qualified System.Directory   as SD
import           System.FilePath    (takeExtension)
import           System.IO          (IOMode (ReadMode), SeekMode (AbsoluteSeek),
                                     hClose, hSeek, withBinaryFile)
import qualified System.IO.Temp     as SIT
import           System.IO.Error    (tryIOError)
import qualified System.Posix.Files as SPF

import           Control.Exception  (bracket)
import           Control.Monad      (when)

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Function      ((&))
import           Data.List          (intercalate)
import           Data.Maybe         (fromJust, isJust)

import           Conduit            (MonadResource, filterC, runConduit,
                                     runResourceT, sinkList)
import qualified Conduit            as C
import           Data.Conduit       (ConduitT, (.|))

import           Polysemy           (Embed, Member, Members, Sem, embed,
                                     embedToFinal, interpret, makeSem, run,
                                     runFinal)
import           Polysemy.Error     (Error)
import qualified Polysemy.Error     as PE
import           Polysemy.Output    (runOutputList)
import           Polysemy.Reader    (Reader, ask)

{-
   File effect to read and write files that can throw an error.  The main idea
   is that the code can read and write files and the errors that are thrown are
   Polysemy errors.
-}


data FileException = FileException FilePath Text
                     | FileExceptions [FileException]

instance Show FileException where
    show ex = "File Handing issue: " ++ ss
      where
          ss = case ex of
              (FileException fp s) -> if null fp
                                        then "Error: " ++ show s
                                        else "FilePath: " ++ show fp ++ ", Error: " ++ show s
              (FileExceptions xs)  -> intercalate ", " $ map show xs


data File m a where
    -- Stat, read and write files as atomic operations
    FileStatus :: FilePath -> File m SPF.FileStatus
    ReadFile :: FilePath -> Maybe Int -> Maybe Int -> File m ByteString
    WriteFile :: FilePath -> ByteString -> File m ()
    DeleteFile :: FilePath -> File m ()
    DoesFileExist :: FilePath -> File m Bool
    CopyFile :: FilePath -> FilePath -> File m ()
    CopyFileWithMetadata :: FilePath -> FilePath -> File m ()

    -- List files in Directories
    SourceDirectory:: FilePath -> File m [FilePath]
    SourceDirectoryDeep:: Bool -> FilePath -> File m [FilePath]
    SourceDirectoryFilter :: FilePath -> (FilePath -> Bool) -> File m [FilePath]
    SourceDirectoryDeepFilter :: Bool -> FilePath -> (FilePath -> Bool) -> File m [FilePath]

    -- Directory support
    MakeAbsolute :: FilePath -> File m FilePath
    DoesDirectoryExist :: FilePath -> File m Bool
    CreateDirectory :: FilePath -> File m ()
    CreateDirectoryIfMissing :: Bool -> FilePath -> File m ()
    RemoveDirectory :: FilePath -> File m ()
    RemoveDirectoryRecursive :: FilePath -> File m ()

    -- Temporary Dirctory/File Support
    GetCanonicalTemporaryDirectory :: File m FilePath
    CreateTempDirectory :: FilePath -> String -> File m FilePath
    EmptyTempFile :: FilePath -> String -> File m FilePath
    EmptySystemTempFile :: String -> File m FilePath


makeSem ''File


fileToIO
    :: Members '[ Error FileException
                , Embed IO
                ] r
    => Sem (File ': r) a
    -> Sem r a
fileToIO = interpret $ \case
    -- Get the FileStatus for a file
    -- fileStatus :: FilePath -> FileStatus
    FileStatus fp ->
        throwIfException fp =<< embed (tryIOError $ SPF.getFileStatus fp)

    -- read a file with optional offset and optional size
    -- readFile :: FilePath -> Maybe Int -> Maybe Int -> ByteString
    ReadFile fp seek size -> do
        res <- embed $ tryIOError $ withBinaryFile fp ReadMode
            (\handle -> do
                when (isJust seek) $ hSeek handle AbsoluteSeek (toInteger (fromJust seek))
                case size of
                    Just size' -> BS.hGet handle size'
                    Nothing    -> BS.hGetContents handle)
        throwIfException fp res

    -- write a ByteString to the filepath
    -- writeFile :: FilePath -> ByteString
    WriteFile fp bs ->
        throwIfException fp =<< embed (tryIOError $ BS.writeFile fp bs)

    -- delete a file represented by the filepath
    -- DeleteFile :: FilePath -> File m ()
    DeleteFile fp ->
        throwIfException fp =<< embed (tryIOError $ SD.removeFile fp)

    --DoesFileExist :: FilePath -> File m Bool
    DoesFileExist fp ->
        throwIfException fp =<< embed ( tryIOError $ SD.doesFileExist fp)


    --CopyFile :: FilePath -> FilePath -> File m ()
    CopyFile fromFp toFp ->
        throwIfException toFp =<< embed (tryIOError $ SD.copyFile fromFp toFp)

    --CopyFileWithMetadata :: FilePath -> FilePath -> File m ()
    CopyFileWithMetadata fromFp toFp ->
        throwIfException toFp =<< embed (tryIOError $ SD.copyFileWithMetadata fromFp toFp)

    -- The next two functions use conduit and return a list of files.
    -- Conduit is used as it's convenient to list the files without using up
    -- lots of filehandles, plus we can run the filter in the stream.

    -- SourceDirectory:: FilePath -> File m [FilePath]
    SourceDirectory dir -> throwIfException dir =<< embed
        ( tryIOError
        $ runResourceT
        $ runConduit
        $ C.sourceDirectory dir .| sinkList)

    -- SourceDirectoryDeep:: Bool -> FilePath -> File m [FilePath]
    SourceDirectoryDeep flag dir ->
        throwIfException dir =<< embed
             ( tryIOError
             $ runResourceT
             $ runConduit
             $ C.sourceDirectoryDeep flag dir .| sinkList)

    -- Do a shallow traverse into the file system at FilePath point.
    -- It returns everything, files, directories, symlinks, etc.
    -- Could throw a FileException if things go wrong
    -- SourceDirectoryDeepFilter
    -- :: FilePath            - the directory to list files from
    -- -> (FilePath -> Bool)  - a filter to apply to each filepath
    -- -> File m [FilePath]   - and return a list of FilePath types
    SourceDirectoryFilter dir filterFunc ->
        throwIfException dir =<< embed
             ( tryIOError
             $ runResourceT
             $ runConduit
             $ C.sourceDirectory dir
                .| filterC filterFunc
                .| sinkList)

    -- Do a deep traverse into the file system at FilePath point.
    -- Could throw a FileException if things go wrong
    -- SourceDirectoryDeepFilter
    -- :: FilePath            - the directory to list files from
    -- -> Bool                - whether to follow symlinks (True = follow)
    -- -> (FilePath -> Bool)  - a filter to apply to each filepath
    -- -> File m [FilePath]   - and return a list of FilePath types
    SourceDirectoryDeepFilter flag dir filterFunc ->
        throwIfException dir =<< embed
             ( tryIOError
             $ runResourceT
             $ runConduit
             $ C.sourceDirectoryDeep flag dir
                .| filterC filterFunc
                .| sinkList)

    -- MakeAbsolute :: FilePath -> File m FilePath
    MakeAbsolute fp ->
        throwIfException fp =<< embed ( tryIOError $ SD.makeAbsolute fp)

    --DoesDirectoryExist :: FilePath -> File m Bool
    DoesDirectoryExist fp ->
        throwIfException fp =<< embed ( tryIOError $ SD.doesDirectoryExist fp)

    -- CreateDirectory :: FilePath -> File m ()
    CreateDirectory fp ->
        throwIfException fp =<< embed (tryIOError $ SD.createDirectory fp)

    -- CreateDirectoryIfMissing :: Bool -> FilePath -> File m ()
    CreateDirectoryIfMissing b fp ->
        throwIfException fp =<< embed (tryIOError $ SD.createDirectoryIfMissing b fp)

    -- RemoveDirectory :: FilePath -> File m ()
    RemoveDirectory fp ->
        throwIfException fp =<< embed (tryIOError $ SD.removeDirectory fp)

    -- RemoveDirectoryRecursive :: FilePath -> File m ()
    RemoveDirectoryRecursive fp ->
        throwIfException fp =<< embed (tryIOError $ SD.removeDirectoryRecursive fp)

    -- Temporary Dirctory/File Support
    -- GetCanonicalTemporaryDirectory :: File m FilePath
    GetCanonicalTemporaryDirectory ->
        throwIfException "" =<< embed (tryIOError SIT.getCanonicalTemporaryDirectory)

    -- CreateTempDirectory :: FilePath -> String -> File m FilePath
    CreateTempDirectory fp pat ->
        throwIfException fp =<< embed (tryIOError $ SIT.createTempDirectory fp pat)

    -- EmptyTempFile :: FilePath -> String -> File m FilePath
    EmptyTempFile fp pat ->
        throwIfException fp =<< embed (tryIOError $ SIT.emptyTempFile fp pat)

    -- EmptySystemTempFile :: FilePath -> String -> File m FilePath
    EmptySystemTempFile pat ->
        throwIfException "" =<< embed (tryIOError $ SIT.emptySystemTempFile pat)


-- helper function to throw an Either a b to a FileException if its a Left or
-- just return the right as a pure.
throwIfException
    :: Member (Error FileException) r
    => TextShow a
    => FilePath
    -> Either a b
    -> Sem r b
throwIfException fp e = case e of
    Left a -> PE.throw $ FileException fp (showt a)
    Right b -> pure b

-- let's do q quick tests -- we'll delete these once we have stuff going!

runTest x = x & fileToIO
              & PE.errorToIOFinal @FileException
              & embedToFinal @IO
              & runFinal


getStatus :: Member File r => FilePath -> Sem r SPF.FileStatus
getStatus = fileStatus


fetchContents :: Member File r => FilePath -> Sem r ByteString
fetchContents fp = readFile fp Nothing Nothing

getStatusP fp = getStatus fp & runTest

fetchContentsP fp = fetchContents fp & runTest
