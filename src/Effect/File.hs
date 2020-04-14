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



module Effect.File
      where

import           Prelude                hiding (readFile)

import           System.FilePath        (takeExtension)
import           System.IO              (IOMode (ReadMode),
                                         SeekMode (AbsoluteSeek), hClose, hSeek,
                                         openBinaryFile)
import           System.IO.Error        (tryIOError)
import qualified System.Posix.Files     as SPF

import           Control.Exception      (bracket)
import           Control.Monad          (when)

import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Function          ((&))
import           Data.List              (intercalate)
import           Data.Maybe             (fromJust, isJust)

import           Polysemy               (Embed, Member, Members, Sem, embed,
                                         embedToFinal, interpret, makeSem, run,
                                         runFinal)
import           Polysemy.Error         (Error)
import qualified Polysemy.Error         as PE
import           Polysemy.Output        (runOutputList)
import           Polysemy.Reader        (Reader, ask)

{-
   File effect to read and write files that can throw an error.  The main idea
   is that the code can read and write files and the errors that are thrown are
   Polysemy errors.
-}


data FileException = FileException String
                     | FileExceptions [FileException]

instance Show FileException where
    show ex = "File Handing issue: " ++ ss
      where
          ss = case ex of
              (FileException s)   -> s
              (FileExceptions xs) -> intercalate ", " $ map show xs


data File m a where
    FileStatus :: FilePath -> File m SPF.FileStatus
    ReadFile :: FilePath -> Maybe Int -> Maybe Int -> File m ByteString
    WriteFile :: FilePath -> ByteString -> File m ()

makeSem ''File


fileToIO :: Members '[ Error FileException
                     , Embed IO
                     ] r
              => Sem (File ': r) a
              -> Sem r a
fileToIO = interpret $ \case
    -- Get the FileStatus for a file
    -- fileStatus :: FilePath -> FileStatus
    FileStatus fp -> do
        fs' <- embed $ tryIOError $ SPF.getFileStatus fp
        case fs' of
            Right fs     -> pure fs
            Left ioerror -> PE.throw $ FileException $ show ioerror

    -- read a file with optional offset and optional size
    -- readFile :: FilePath -> Maybe Int -> Maybe Int -> ByteString
    ReadFile fp seek size -> do
        res' <- embed $ tryIOError $ bracket
            (openBinaryFile fp ReadMode)
            (hClose)
            (\handle -> do
                when (isJust seek) $ hSeek handle AbsoluteSeek (toInteger (fromJust seek))
                case size of
                    Just size' -> BS.hGet handle size'
                    Nothing    -> BS.hGetContents handle)
        case res' of
            Right res    -> pure res
            Left ioerror -> PE.throw $ FileException $ show ioerror

    -- write a ByteString to the filepath
    -- writeFile :: FilePath -> ByteString
    WriteFile fp bs -> do
        res' <- embed $ tryIOError $ BS.writeFile fp bs
        case res' of
            Right _      -> pure ()
            Left ioerror -> PE.throw $ FileException $ show ioerror

-- let's do q quick tests -- we'll delete these once we have stuff going!

runTest x = x & fileToIO
              & PE.errorToIOFinal @FileException
              & embedToFinal @IO
               & runFinal


getStatus :: Member File r => FilePath -> Sem r SPF.FileStatus
getStatus fp = fileStatus fp


fetchContents :: Member File r => FilePath -> Sem r ByteString
fetchContents fp = readFile fp Nothing Nothing

getStatusP fp = getStatus fp & runTest

fetchContentsP fp = fetchContents fp & runTest
