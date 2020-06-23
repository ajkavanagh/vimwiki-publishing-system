{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.Functions where

import           System.FilePath.Posix  (normalise, (</>))

import           Data.Maybe             (isNothing)
import           Data.Text              (Text, unpack)

import           Text.Ginger            ((~>))
import qualified Text.Ginger            as TG

import qualified Network.URI            as NU

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy   as CP
import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)
import           Polysemy.Writer        (Writer)

import           Effect.ByteStringStore (ByteStringStore)
import           Effect.File            (File)

import           Lib.Context.Core       (Context, RunSem, RunSemGVal,
                                         contextFromList, tryExtractStringArg)
import           Lib.Errors             (SiteGenError)
import qualified Lib.Header             as H
import           Lib.Pandoc             (scContentM, scSummaryM, scTocM)
import           Lib.SiteGenConfig      (SiteGenConfig (..))
import           Lib.SiteGenState       (SiteGenReader, SiteGenState)


functionsContext
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => Context (RunSem r)
functionsContext = contextFromList
    [ ("absURL",  pure $ TG.fromFunction absURL)
    ]


absURL
    :: ( Member File r
       , Member ByteStringStore r
       , Member (State SiteGenState) r
       , Member (Reader SiteGenReader) r
       , Member (Reader SiteGenConfig) r
       , Member (Error SiteGenError) r
       , Member (Log String) r
       )
    => TG.Function (RunSem r)
absURL args = do
    mSiteUri <- TG.liftRun $ PR.asks @SiteGenConfig sgcSiteUrl
    let mArg = NU.parseURIReference =<< unpack <$> tryExtractStringArg args -- try to get the relative URI
    case mSiteUri of
        -- if there wasn't a site url, just return the parsed version of the
        -- existing one
        Nothing -> pure $ TG.toGVal (show <$> mArg)
        -- there is a site abs URL.  We want to use that, keeping the scheme,
        -- authority and path, but adding in the path, query and fragment from
        -- the passed arg, but ONLY if the arg isn't absolute.
        Just uri -> case mArg of
            Nothing -> pure $ TG.toGVal (Nothing :: Maybe String)
            Just arg ->
                if NU.uriIsAbsolute arg
                    -- if the passed URI is absolute, just return it
                    then pure $ TG.toGVal (show arg)
                    else pure $ TG.toGVal (show
                        $ arg { NU.uriScheme=NU.uriScheme uri
                              , NU.uriAuthority=NU.uriAuthority uri
                              , NU.uriPath=normalise(NU.uriPath uri </> NU.uriPath arg)
                              , NU.uriQuery=NU.uriQuery arg
                              , NU.uriFragment=NU.uriFragment arg })
