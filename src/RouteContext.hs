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

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module RouteContext where


import           Data.List             (intercalate)
import           Data.Text.Titlecase   (titlecase)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, systemToPOSIXTime)
import           System.FilePath       (FilePath, pathSeparator, takeDirectory,
                                        (</>))
import qualified System.FilePath       as FP
import qualified System.Posix.Files    as SPF

import           Polysemy              (Members, Sem)
import           Polysemy.Error        (Error)

import           Effect.File           (File, FileException (..), fileStatus,
                                        fileToIO)

{-
   The RouteContext is build from the filename and file details.

   * The slug is the path from the site route split as directory names to the
     path of the slug.  The filename, if it contains spaces, is converted to '-'
     characters.
   * The file time is obtained from the file.
   * The filepath is the normalised to the site directory.
   * the autoTitle is obtained from the file name and path with spaces included,
     but stripping off the extension.

   It's stored as a context item purely so that it's easy to use in the code and
   can be provided to the renderer if needed.
-}


data RouteContext = RouteContext
    { rcAutoSlug  :: !String
    , rcFileTime  :: !UTCTime
    , rcFileName  :: !FilePath
    , rcAutoTitle :: !String
    } deriving Show


data FilePathParts = FilePathParts
    { _fileName   :: !String
    , _path       :: ![String]
    , _normalised :: !FilePath
    } deriving Show


makeRouteContextFromFileName :: Members '[ File
                                         , Error FileException
                                         ] r
                             => FilePath
                             -> Sem r RouteContext
makeRouteContextFromFileName fp = do
    fs <- fileStatus fp
    let fpp = decodeFilePath fp
        time = posixSecondsToUTCTime $ SPF.modificationTimeHiRes fs
    pure $ RouteContext { rcAutoSlug = makeAutoSlug fpp
                        , rcFileTime = time
                        , rcFileName = _normalised fpp
                        , rcAutoTitle = makeAutoTitle fpp
                        }


decodeFilePath :: FilePath -> FilePathParts
decodeFilePath fp =
    let _normalised = FP.normalise fp
        noExt = FP.dropExtensions fp
        parts = FP.splitDirectories noExt
     in FilePathParts { _fileName = FP.takeFileName _normalised
                      , _path = parts
                      , _normalised = _normalised
                      }


makeAutoSlug :: FilePathParts -> String
makeAutoSlug fpp =
    let repl ' ' = '-'
        repl c   = c
        ps = map (map repl) $ _path fpp
     in FP.joinPath ps


makeAutoTitle :: FilePathParts -> String
makeAutoTitle fpp =
    let tParts = map titlecase $ _path fpp
     in intercalate " / " tParts
