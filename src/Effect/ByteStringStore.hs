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

-- | The Fragment effect is for storing blobs of stuff against a key.  This is
-- to allow (in this case) the parsed and rendered HTML bits (from Pandoc) to be
-- stored so that they are availble for inserting into Ginger templates.  In
-- particular, this is driven by the summary html fragment so that we don't
-- parse the markdown (and do all the clean up) several times.

module Effect.ByteStringStore
      where

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Control.Monad       (forM_)

import           System.FilePath     ((</>))
import qualified System.FilePath     as F
import qualified System.Posix.Files  as SPF

import           Polysemy            (Embed, Member, Members, Sem, embed,
                                      embedToFinal, interpret, makeSem, run,
                                      runFinal)
import           Polysemy.Error      (Error)
import qualified Polysemy.Error      as PE
import           Polysemy.Reader     (Reader)
import qualified Polysemy.Reader     as PR
import           Polysemy.State      (State)
import qualified Polysemy.State      as PS

import           Effect.File         (File, FileException)
import qualified Effect.File         as EF

import qualified Lib.SiteGenConfig   as S


data ByteStringStore m a where
    Store :: Text -> ByteString -> ByteStringStore m ()
    Fetch :: Text -> ByteStringStore m (Maybe ByteString)
    Clear :: Text -> ByteStringStore m ()
    ClearAll :: ByteStringStore m ()


makeSem ''ByteStringStore


type BSHMStore = HashMap Text ByteString


bsStoreAsHash :: Member (State BSHMStore) r
              => Sem (ByteStringStore ': r) a
              -> Sem r a
bsStoreAsHash = interpret $ \case
    -- Store the bytestring in the Hash in the State
    -- Store :: Text -> ByteString -> ByteStringStore m ()
    Store key bs -> PS.modify $ \m -> HashMap.insert key bs m
    -- Fetch the value from the store if it exists
    -- Fetch :: Text -> ByteStringStore m (Maybe ByteString)
    Fetch key -> HashMap.lookup key <$> PS.get
    -- Clear the hash of a key.
    -- Clear :: Text -> ByteStringStore m ()
    Clear key -> PS.modify @BSHMStore $ \m -> HashMap.delete key m
    -- ClearAll the keys from the store
    -- ClearAll :: ByteStringStore m ()
    ClearAll -> PS.put @BSHMStore HashMap.empty


type BSHMFile = HashMap Text FilePath


bsStoreAsFile :: ( Member (State BSHMFile) r
                 , Member File r
                 , Member (Error FileException) r
                 )
              => FilePath
              -> Sem (ByteStringStore ': r) a
              -> Sem r a
bsStoreAsFile tempDir m = do
    -- ensure the tempDir exists
    exists <- EF.doesDirectoryExist tempDir
    if not exists
      then PE.throw $ EF.FileException tempDir "Temp directory doesn't exist: "
      else interpret (\case
        -- Store the bytestring in the Hash in the State
        -- Store :: Text -> ByteString -> ByteStringStore m ()
        Store key bs ->  do
            let fp = tempDir </> makeFileFromKey key
            PS.modify $ \m -> HashMap.insert key fp m
            EF.writeFile fp bs
        -- Fetch the value from the store if it exists
        -- Fetch :: Text -> ByteStringStore m (Maybe ByteString)
        Fetch key ->  do
            let fp = tempDir </> makeFileFromKey key
            PE.catch @FileException (Just <$> EF.readFile fp Nothing Nothing)
                                    (\_ -> pure Nothing)
        -- Clear the hash of a key.
        -- Clear :: Text -> ByteStringStore m ()
        Clear key -> do
            let fp = tempDir </> makeFileFromKey key
            PS.modify @BSHMFile $ \m -> HashMap.delete key m
            PE.catch @FileException (EF.deleteFile fp) (\_ -> pure ())
        -- ClearAll the keys from the store
        -- ClearAll :: ByteStringStore m ()
        ClearAll -> do
            vs <- HashMap.elems <$> PS.get @BSHMFile
            forM_ vs $ \fp ->
                PE.catch @FileException (EF.deleteFile fp) (\_ -> pure ())
            PS.put @BSHMFile HashMap.empty
        ) m


makeFileFromKey :: Text -> FilePath
makeFileFromKey txt =
    let sTxt = T.unpack txt
     in fmap go sTxt
  where
    go '/' = '_'
    go '.' = '_'
    go x   = x
