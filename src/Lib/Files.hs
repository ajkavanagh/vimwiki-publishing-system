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



module Lib.Files
    ( sourceDirectory
    , filePathToSourcePageHeaders
    , filePathToMaybeSourcePageHeader
    ) where


import           System.FilePath    (takeExtension)
import           System.Posix.Files (fileSize)

import           Control.Monad      (filterM, liftM, unless, (>=>))

import           Data.ByteString    (ByteString)
import qualified Data.ByteString    as BS
import           Data.Function      ((&))
import           Data.Maybe         (catMaybes)

import           Colog.Core         (logStringStderr)
import           Colog.Polysemy     (Log, runLogAction)
import qualified Colog.Polysemy     as CP
import           Polysemy           (Embed, Members, Sem, embed, embedToFinal,
                                     run, runFinal)
import           Polysemy.Error     (Error, throw)
import qualified Polysemy.Error     as PE
import           Polysemy.Reader    (Reader, runReader)

import           Effect.File        (File, FileException (..))
import qualified Effect.File        as EF

import           Lib.Header         (SourcePageHeader, maxHeaderSize,
                                     maybeDecodeHeader)
import           Lib.Utils          (strToLower)
import           Lib.SiteGenConfig  (maxFileToProcessSize)
import qualified Lib.SiteGenConfig  as S

import qualified Lib.RouteContext   as R



-- | Get a list of files for a directory (the FilePath) and an extension
-- (including the '.').
sourceDirectory
    :: Members '[ File
                , Error FileException
                , Log String
                ] r
    => FilePath
    -> String
    -> Sem r [FilePath]
sourceDirectory dir ext = do
    paths <- EF.sourceDirectoryDeepFilter False dir (isExtensionFileLC lExt)
    filterM (isSmallerThanM maxFileToProcessSize) paths
  where
    lExt = strToLower ext


-- | return True fi the extension on the file matches (lowercased) to the
-- filepath provided.
isExtensionFileLC :: String -> FilePath -> Bool
isExtensionFileLC ext = (==ext) . strToLower . takeExtension


-- return True if the filepath file is <= the size provided
-- runs inside a monad that has IO.
isSmallerThanM
    :: Members '[ File
                , Error FileException
                , Log String
                ] r
    => Int
    -> FilePath
    -> Sem r Bool
isSmallerThanM size fp = do
    fs <- EF.fileStatus fp
    let size' = (fromIntegral . fileSize) fs
    let ok = size' <= size
    unless ok $ CP.log @String $ "File " ++ show fp ++ " is too big to process"
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
    bs <- EF.readFile fp Nothing (Just maxHeaderSize)  -- read up to maxHeaderSize bytes
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
filePathsToSourcePageHeaders fs =
    catMaybes <$> mapM filePathToMaybeSourcePageHeader fs


-- finally, we convert a single FilePath (a directory) to a list of
-- SourcePageHeaders.  This is the magic function to find out what we need to
-- process.
filePathToSourcePageHeaders
    :: Members '[ File
                , Error FileException
                , Reader S.SiteGenConfig
                , Log String
                ] r
    => FilePath       -- the directory
    -> String         -- the extension to filter by
    -> Sem r [SourcePageHeader]
filePathToSourcePageHeaders dir ext =
    filePathsToSourcePageHeaders =<< sourceDirectory dir ext



-- Some tests; we'll delete these when we move to unit testing this module.
-- test if we can get a file list

-- TODO: remove these test functions

runTest x = x & EF.fileToIO
              & PE.errorToIOFinal @FileException
              & runLogAction @IO logStringStderr  -- [Embed IO, Error ConfigException]
              & embedToFinal @IO
              & runFinal

sourceDirectoryP dir ext = sourceDirectory dir ext & runTest
