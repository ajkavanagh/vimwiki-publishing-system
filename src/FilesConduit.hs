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



module FilesConduit
      where


import           System.FilePath    (takeExtension)
import           System.IO.Error    (tryIOError)
import           System.Posix.Files (fileSize)

import           Control.Monad      (filterM, when)

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Function      ((&))

import           Conduit            (MonadResource, filterC, runConduit,
                                     runResourceT, sinkList,
                                     sourceDirectoryDeep)
import           Data.Conduit       (ConduitT, (.|))

import           Colog.Core         (logStringStderr)
import           Colog.Polysemy     (Log, runLogAction)
import qualified Colog.Polysemy     as CP
import           Polysemy           (Embed, Members, Sem, embed, embedToFinal,
                                     run, runFinal)
import           Polysemy.Error     (Error, throw)
import qualified Polysemy.Error     as PE

import           Effect.File        (File, FileException (..), fileStatus,
                                     fileToIO)
import           Header             (SourcePageHeader, maxHeaderSize,
                                     maybeDecodeHeader)
import           Lib                (strToLower)
import qualified RouteContext       as R
import           SiteGenConfig      (maxFileToProcessSize)
import qualified SiteGenConfig      as S



-- | Get a list of files for a directory (the FilePath) and an extension
-- (including the '.').  This uses conduit, but returns a list.  This was
-- because it was proving very tricky for me to thread the Sem r monad through
-- the ConduitT and in the end I just decided to take the hit on memory, but use
-- Conduit to save on file handles.
sourceFiles :: Members '[ File
                        , Error FileException
                        , Log String
                        , Embed IO
                        ] r
            => FilePath
            -> String
            -> Sem r [FilePath]
sourceFiles fp ext = do
    res <- embed $ tryIOError $ runResourceT $ runConduit $ sourceFilesC fp ext .| sinkList
    -- still need to process each of the possible md files to check they are not
    -- too big
    case res of
        Left ioerror -> throw $ FileException (show ioerror)
        Right paths -> do
            rs <- filterM (isSmallerThan maxFileToProcessSize) paths
            pure rs

-- filterM :: Applicative m => (a -> m Bool) -> [a] -> m [a]


sourceFilesC :: MonadResource m => FilePath -> String -> ConduitT () FilePath m ()
sourceFilesC dir ext = sourceDirectoryDeep False dir
                    .| filterC (isExtensionFile lExt)
  where
      lExt = strToLower ext



isExtensionFile :: String -> FilePath -> Bool
isExtensionFile ext = (==ext) . strToLower . takeExtension


-- return True if the filepath file is <= the size provided
-- runs inside a monad that has IO.
isSmallerThan :: Members '[ File
                          , Error FileException
                          , Log String
                          , Embed IO
                          ] r
              => Int
              -> FilePath
              -> Sem r Bool
isSmallerThan size fp = do
    fs <- fileStatus fp
    let size' = (fromIntegral . fileSize) fs
    let ok = size' <= size
    when (not ok) $ CP.log @String $ "File " ++ (show fp) ++ " is too big to process"
    pure ok




-- | convert the FilePath -> (FilePath, conduit of file data)

{- what we want to do is to read enough of the file to determine:

   1. Does it start with the header (from Header.hs)
   2. Can we read data until we find the end header (or not).

   We might have to read the entire file, but hopefully we can read enough data
   to pass the block to the extract header function.

   We also want to run this in the Sem r monad so that we can embed the IO to
   read enough data.

   HERE

-}


-- test if we can get a file list


runTest x = x & fileToIO
              & PE.errorToIOFinal @FileException
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal

sourceFilesP fp ext = sourceFiles fp ext & runTest
