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

module Effect.Cache
      where

import           Prelude             hiding (readFile)

import           TextShow

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Control.Monad       (forM_)

import           System.FilePath     ((</>))

import           Polysemy            (Member, Sem, interpret, makeSem, runFinal)
import           Polysemy.Error      (Error)
import qualified Polysemy.Error      as PE
import           Polysemy.State      (State)
import qualified Polysemy.State      as PS

import           Effect.File         (File, FileException)
import qualified Effect.File         as EF


data Cache b m a where
    Store :: Text -> b -> Cache b m ()
    Fetch :: Text -> Cache b m (Maybe b)
    Clear :: Text -> Cache b m ()
    ClearAll :: Cache b m ()


makeSem ''Cache

type CacheStore b = HashMap Text b


cacheInHash :: forall b r a. Member (State (CacheStore b)) r
            => Sem (Cache b ': r) a
            -> Sem r a
cacheInHash = interpret $ \case
    -- Store the b in the Hash in the State
    -- Store :: Text -> b -> Cache b m ()
    Store key d -> PS.modify $ \m -> HashMap.insert key d m
    -- Fetch the value from the store if it exists
    -- Fetch :: Text -> Cache b m (Maybe b)
    Fetch key -> HashMap.lookup key <$> PS.get
    -- Clear the hash of a key.
    -- Clear :: Text -> Cache b m ()
    Clear key -> PS.modify @(CacheStore b) $ \m -> HashMap.delete key m
    -- ClearAll the keys from the store
    -- ClearAll :: Cache b m ()
    ClearAll -> PS.put @(CacheStore b) HashMap.empty



type CacheFileMap = HashMap Text FilePath


cacheInFiles :: ( Member (State CacheFileMap) r
                , Member File r
                , Member (Error FileException) r
                )
              => FilePath
              -> (b -> ByteString)
              -> (ByteString -> b)
              -> Sem (Cache b ': r) a
              -> Sem r a
cacheInFiles tempDir encode decode m = do
    -- ensure the tempDir exists
    exists <- EF.doesDirectoryExist tempDir
    if not exists
      then PE.throw $ EF.FileException tempDir "Temp directory doesn't exist: "
      else interpret (\case
        -- Store the b in the Hash in the State
        -- Store :: Text -> b -> Cache b m ()
        Store key d ->  do
            let fp = tempDir </> makeFileFromKey key
            PS.modify $ \m' -> HashMap.insert key fp m'
            EF.writeFile fp (encode d)
        -- Fetch the value from the store if it exists
        -- Fetch :: Text -> Cache b m (Maybe b)
        Fetch key ->  do
            let fp = tempDir </> makeFileFromKey key
            PE.catch @FileException (Just . decode <$> EF.readFile fp Nothing Nothing)
                                    (\_ -> pure Nothing)
        -- Clear the hash of a key.
        -- Clear :: Text -> Cache b m ()
        Clear key -> do
            let fp = tempDir </> makeFileFromKey key
            PS.modify @CacheFileMap $ \m -> HashMap.delete key m
            PE.catch @FileException (EF.deleteFile fp) (\_ -> pure ())
        -- ClearAll the keys from the store
        -- ClearAll :: Cache b m ()
        ClearAll -> do
            vs <- HashMap.elems <$> PS.get @CacheFileMap
            forM_ vs $ \fp ->
                PE.catch @FileException (EF.deleteFile fp) (\_ -> pure ())
            PS.put @CacheFileMap HashMap.empty
        ) m


makeFileFromKey :: Text -> FilePath
makeFileFromKey txt =
    let sTxt = T.unpack txt
     in fmap go sTxt
  where
    go '/' = '_'
    go '.' = '_'
    go x   = x
