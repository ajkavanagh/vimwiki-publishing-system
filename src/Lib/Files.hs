{-# LANGUAGE ConstraintKinds      #-}
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
    , filePathToSourceMetadataItems
    , filePathToMaybeSourceMetadata
    , ensureDirectoriesExistFor
    , writeAndMemo
    , copyStaticFiles
    ) where


import           System.FilePath.Posix (joinPath, makeRelative, normalise,
                                        splitDirectories, takeDirectory,
                                        takeExtension, (</>))
import           System.Posix.Files    (fileSize)

import           Control.Monad         (filterM, forM_, liftM, unless, when,
                                        (>=>))

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import           Data.Function         ((&))
import qualified Data.List             as L
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Encoding    (encodeUtf8)

import           Colog.Core            (logStringStderr)
import           Colog.Polysemy        (Log, runLogAction)
import qualified Colog.Polysemy        as CP
import           Polysemy              (Embed, Member, Members, Sem, embed,
                                        embedToFinal, run, runFinal)
import           Polysemy.Error        (Error, throw)
import qualified Polysemy.Error        as PE
import           Polysemy.Reader       (Reader)
import qualified Polysemy.Reader       as PR
import           Polysemy.State        (State)
import qualified Polysemy.State        as PS

import           Effect.File           (File, FileException (..))
import qualified Effect.File           as EF
import           Effect.Logging        (LoggingMessage)
import qualified Effect.Logging        as EL

import           Types.Errors          (SiteGenError)
import           Types.Header          (SourceMetadata (..))
import           Types.SiteGenState    (FileMemo (..), SiteGenState (..))

import           Lib.Header            (makeHeaderContextFromFileName,
                                        maxHeaderSize, maybeDecodeHeader)
import           Lib.SiteGenConfig     (SiteGenConfig (..),
                                        maxFileToProcessSize)
import           Lib.SiteGenState      (recordMemo)
import           Lib.Utils             (strToLower)


type RestrictedFilesSemEffects r
    = ( Member File r
      , Member (Error FileException) r
      , Member (Reader SiteGenConfig) r
      , Member (Log LoggingMessage) r
      )


type FilesSemEffects r
    = ( Member File r
      , Member (Error FileException) r
      , Member (Error SiteGenError) r
      , Member (Reader SiteGenConfig) r
      , Member (Log LoggingMessage) r
      , Member (State SiteGenState) r
      )


-- | Get a list of files for a directory (the FilePath) and an extension
-- (including the '.').
sourceDirectory
    :: RestrictedFilesSemEffects r
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
    :: RestrictedFilesSemEffects r
    => Int
    -> FilePath
    -> Sem r Bool
isSmallerThanM size fp = do
    fs <- EF.fileStatus fp
    let size' = (fromIntegral . fileSize) fs
    let ok = size' <= size
    unless ok $ EL.logError $ T.pack $ "File " ++ show fp ++ " is too big to process"
    pure ok




-- | convert the FilePath -> Maybe SourceMetadata

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
filePathToMaybeSourceMetadata
    :: RestrictedFilesSemEffects r
    => FilePath
    -> Sem r (Maybe SourceMetadata)
filePathToMaybeSourceMetadata fp = do
    sfp <- PR.asks @SiteGenConfig sgcSource
    hc <- makeHeaderContextFromFileName sfp fp
    bs <- EF.readFile fp Nothing (Just maxHeaderSize)  -- read up to maxHeaderSize bytes
    PR.runReader hc $ maybeDecodeHeader bs   -- add in The Reader HeaderContext to the Sem monad


-- now convert a bunch of files to a list of SourceMetadata items -- note the
-- list may be empty if there are not headers available, or the files do not
-- resolve.
filePathsToSourceMetadata
    :: RestrictedFilesSemEffects r
    => [FilePath]
    -> Sem r [SourceMetadata]
filePathsToSourceMetadata fs =
    catMaybes <$> mapM filePathToMaybeSourceMetadata fs


-- finally, we convert a single FilePath (a directory) to a list of
-- SourceMetadata items.  This is the magic function to find out what we need to
-- process.
filePathToSourceMetadataItems
    :: RestrictedFilesSemEffects r
    => FilePath       -- the directory
    -> String         -- the extension to filter by
    -> Sem r [SourceMetadata]
filePathToSourceMetadataItems dir ext =
    filePathsToSourceMetadata =<< sourceDirectory dir ext


-- | copy static files from the statics dir to the target directory.  Print
-- warnings if we overwrite something that we've generated (i.e. in the memo).
-- Record what we copied.  This function is only called for its side-effects
copyStaticFiles
    :: FilesSemEffects r
    => Sem r ()
copyStaticFiles = do
    sgc <- PR.ask @SiteGenConfig
    let sourceDir = sgcStaticDir sgc
        targetDir = sgcOutputDir sgc
    -- get all the directories in the statics directory, but don't follow
    -- any symlinks
    paths <-  EF.sourceDirectoryDeep False sourceDir
    let paths' = makeRelative sourceDir <$> paths
    EL.logInfo $ T.pack $ "Copying static files from '" <> sourceDir <> "' to '" <> targetDir <> "':"
    forM_ paths' (copyAndMemoFile sourceDir targetDir)
    EL.logInfo "...done copying."


copyAndMemoFile
    :: FilesSemEffects r
    => FilePath
    -> FilePath
    -> FilePath
    -> Sem r ()
copyAndMemoFile sourceDir targetDir path = do
    let srcFile = normalise (sourceDir </> path)
        toFile  = normalise (targetDir </> path)
    EL.logDebug $ T.pack $ "path is: " <> path
    ensureDirectoriesExistFor targetDir path
    EL.logInfo $ T.pack $ "Copying file " <> srcFile <> " to " <> toFile
    EF.copyFile srcFile toFile
    recordMemo $ FileMemo toFile


-- | ensure that the directories exist for the file we are about to write.  If
-- they don't then create them.  If we can't then try to create them; any errors
-- will cause a File Exception.  The function also makes a note of all the
-- intermediate directories in the memoFiles state (in SiteGenState)
ensureDirectoriesExistFor
    :: FilesSemEffects r
    => FilePath     -- The base directory; we've already checked, but doing again does no harm
    -> FilePath     -- the whole additional path including the file
    -> Sem r ()     -- this function is run for it's side-effect.
ensureDirectoriesExistFor base relFile = do
    let dir = takeDirectory (normalise relFile)
        paths = tail (L.inits (splitDirectories dir))
    -- ensure the directories all exist
    forM_ paths $ \pathList -> do
        let path = normalise (joinPath pathList)
            dir' = normalise (base </> path)
        exists <- EF.doesDirectoryExist dir'
        unless exists $ do
            EL.logDebug $ T.pack $ "Directory " <> dir' <> " doesn't exist, creating"
            EF.createDirectory dir'
        -- as the directory create worked (no exception), or alreadly exists,
        -- create the memo for it.
        recordMemo $ DirMemo path


-- | write the file and memo it, so we know which file was written.
writeAndMemo
    :: FilesSemEffects r
    => FilePath
    -> FilePath
    -> Text
    -> Sem r ()
writeAndMemo dir relFile txt = do
    let fullpath = normalise (dir </> relFile)
        nFile = normalise relFile
    EL.logDebug $ T.pack $ "Writing output to " <> nFile
    EF.writeFile fullpath (encodeUtf8 txt)
    -- if the above passed, the memo the file
    recordMemo $ FileMemo nFile
