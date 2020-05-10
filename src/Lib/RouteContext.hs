{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
--{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.RouteContext
    ( RouteContext(..)
    , makeRouteContextFromFileName
    ) where


import           Data.List             (intercalate)
import           Data.Text.Titlecase   (titlecase)
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        systemToPOSIXTime)
import           System.FilePath       (FilePath, pathSeparator, takeDirectory,
                                        (</>))
import qualified System.FilePath       as FP
import qualified System.Posix.Files    as SPF

import           Polysemy              (Members, Sem)
import           Polysemy.Error        (Error)

import           Effect.File           (File, FileException (..), fileStatus)

import           Lib.Utils             (fixRoute, strToLower)

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
    { rcAutoSlug        :: !String   -- slug created from the filepath
    , rcFileTime        :: !UTCTime  -- The time extracted from the file system
    , rcAbsFilePath     :: !FilePath -- the absolute file path for the filesystem
    , rcRelFilePath     :: !FilePath -- the relative file path to the site src
    , rcVimWikiLinkPath :: !String   -- the path that vimwiki would use
    , rcAutoTitle       :: !String   -- a title created from the rel file path
    } deriving Show


data FilePathParts = FilePathParts
    { _fileName    :: !String
    , _vimWikiLink :: !String
    , _path        :: ![String]
    , _normalised  :: !FilePath
    } deriving Show


makeRouteContextFromFileName
    :: Members '[ File
                , Error FileException
                ] r
    => FilePath         -- the source directory of the files (absolute)
    -> FilePath         -- the filepath  (abs) of the file
    -> Sem r RouteContext
makeRouteContextFromFileName sfp afp = do
    fs <- fileStatus afp
    let rfp = FP.makeRelative sfp afp
    let fpp = decodeFilePath rfp
        time = posixSecondsToUTCTime $ SPF.modificationTimeHiRes fs
    pure $ RouteContext { rcAutoSlug=makeAutoSlug fpp
                        , rcFileTime=time
                        , rcAbsFilePath=afp
                        , rcRelFilePath=_normalised fpp
                        , rcVimWikiLinkPath=_vimWikiLink fpp
                        , rcAutoTitle = makeAutoTitle fpp
                        }


-- decode the relative filepath into parts
decodeFilePath :: FilePath -> FilePathParts
decodeFilePath fp =
    let _normalised = FP.normalise fp
        noExt = FP.dropExtensions fp
        parts = FP.splitDirectories noExt
     in FilePathParts { _fileName = FP.takeFileName _normalised
                      , _vimWikiLink = strToLower noExt   -- file names are lowered
                      , _path = parts
                      , _normalised = _normalised
                      }


-- | Make a slug from the file path parts, ensuring it is lower case and spaces
-- and underscores are replaced by '-'s
makeAutoSlug :: FilePathParts -> String
makeAutoSlug fpp = FP.joinPath $ map fixRoute $ _path fpp


makeAutoTitle :: FilePathParts -> String
makeAutoTitle fpp =
    let tParts = map titlecase $ _path fpp
     in intercalate " / " tParts
