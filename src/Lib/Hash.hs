{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Lib.Hash where

-- This is the Hashing library that provides File detection and changing is done
-- This module provides the functions to hash the file, do comparisons, and
-- generally tell you whether a file has changed, if given the filename.  It
-- doesn't store anything, however.
--
-- It does use the File effect to access the file.

import           Crypto.Hash    (Digest, MD5, hash)

import           Data.Text      (Text)
import           Data.Text      as T

import           Polysemy       (Member, Members, Sem)
import           Polysemy.Error (Error)
import qualified Polysemy.Error as PE

import           Types.Errors   (SiteGenError)

import           Effect.File    (File, FileException)
import qualified Effect.File    as EF


hashFile
    :: ( Member (Error FileException) r
       , Member (Error SiteGenError) r
       , Member File r
       )
    => FilePath
    -> Sem r Text
hashFile fp = do
    bs <- EF.readFile fp Nothing Nothing
    let digest = hash bs :: Digest MD5
    pure $ T.pack $ show digest


