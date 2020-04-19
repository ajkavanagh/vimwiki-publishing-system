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
import           Data.Maybe         (catMaybes)

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
import           Polysemy.Reader    (Reader, runReader)

import           Effect.File        (File, FileException (..), fileToIO)
import qualified Effect.File        as EF
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
    fs <- EF.fileStatus fp
    let size' = (fromIntegral . fileSize) fs
    let ok = size' <= size
    when (not ok) $ CP.log @String $ "File " ++ (show fp) ++ " is too big to process"
    pure ok




-- | convert the FilePath -> Maybe SourcePageHeader

{- what we want to do is to read enough of the file to determine:

   1. Does it start with the header (from Header.hs)
   2. Can we read data until we find the end header (or not).

   We might have to read the entire file, but hopefully we can read enough data
   to pass the block to the extract header function.

   We also want to run this in the Sem r monad so that we can embed the IO to
   read enough data.

   HERE

-}

-- read up to maxHeaderSize of the file (into a ByteString) and see if we can
-- extract a header.
filePathToMaybeSourcePageHeader
    :: Members '[ File
                , Error FileException
                , Reader S.SiteGenConfig
                , Log String
                ] r
    => FilePath
    -> Sem r (Maybe SourcePageHeader)
filePathToMaybeSourcePageHeader fp = do
    rc <- R.makeRouteContextFromFileName fp
    bs <- EF.readFile fp Nothing (Just maxHeaderSize)
    runReader rc $ maybeDecodeHeader bs   -- add in The Reader RouteContext to the Sem monad


-- now convert a bunch of files to a list of SourcePageHeaders -- note the list
-- may be empty if there are not headers available, or the files do not resolve.
filePathsToSourcePageHeaders
    :: Members '[ File
                , Error FileException
                , Reader S.SiteGenConfig
                , Log String
                ] r
    => [FilePath]
    -> Sem r [SourcePageHeader]
filePathsToSourcePageHeaders fs = do
    mSPHs <- mapM filePathToMaybeSourcePageHeader fs
    pure $ catMaybes mSPHs


-- finally, we convert a single FilePath (a directory) to a list of
-- SourcePageHeaders.  This is the magic function to find out what we need to
-- process.
filePathToSourcePageHeaders
    :: Members '[ File
                , Error FileException
                , Reader S.SiteGenConfig
                , Log String
                , Embed IO
                ] r
    => FilePath       -- the directory
    -> String         -- the extension to filter by
    -> Sem r [SourcePageHeader]
filePathToSourcePageHeaders fp ext = do
    fs <- sourceFiles fp ext
    filePathsToSourcePageHeaders fs



-- test if we can get a file list


runTest x = x & fileToIO
              & PE.errorToIOFinal @FileException
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal

sourceFilesP fp ext = sourceFiles fp ext & runTest
