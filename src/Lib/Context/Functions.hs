{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExtendedDefaultRules  #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}


module Lib.Context.Functions where

import           System.FilePath.Posix  (normalise, (</>))

import           Data.Default           (def)
import           Data.Maybe             (isNothing)
import           Data.Text              (Text, unpack)

import           Text.Ginger            ((~>))
import qualified Text.Ginger            as TG

import qualified Network.URI            as NU

import           Colog.Polysemy         (Log)
import qualified Colog.Polysemy         as CP
import           Polysemy               (Member, Sem)
import           Polysemy.Error         (Error)
import           Polysemy.Reader        (Reader)
import qualified Polysemy.Reader        as PR
import           Polysemy.State         (State)
import           Polysemy.Writer        (Writer)

import           Effect.ByteStringStore (ByteStringStore)
import           Effect.File            (File)
import           Effect.Ginger          (GingerSemEffects)

import           Lib.Context.Core       (contextFromList, extractBoolArg,
                                         tryExtractStringArg)
import           Lib.Errors             (SiteGenError)
import qualified Lib.Header             as H
import           Lib.Pandoc             (scContentM, scSummaryM, scTocM)
import           Lib.SiteGenConfig      (SiteGenConfig (..))
import           Lib.SiteGenState       (SiteGenReader, SiteGenState)
import           Types.Context          (Context, RunSem, RunSemGVal)


functionsContext
    :: GingerSemEffects r
    => Context (RunSem r)
functionsContext = contextFromList
    [ ("absURL",    pure $ TG.fromFunction absURLF)
    , ("not",       pure $ TG.fromFunction notF)
    , ("getlocale", pure $ TG.fromFunction getLocaleF)
    ]


absURLF :: GingerSemEffects r => TG.Function (RunSem r)
absURLF args = do
    mSiteUri <- TG.liftRun (PR.asks @SiteGenConfig sgcSiteUrl)
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


notF :: GingerSemEffects r => TG.Function (RunSem r)
notF args = pure $ TG.toGVal $ not $ extractBoolArg args


-- | getLocaleF is needed by the Ginger builtin 'getlocale', but the app has to
-- supply it.  This function is the
-- "modified-for-this-app-but-copied-from-Sprinkles" function from:
-- src/Web/Sprinkles/TemplateContext.hs (https://github.com/tdammers/sprinkles)
getLocaleF :: GingerSemEffects r => TG.Function (RunSem r)
getLocaleF args =
    case TG.extractArgsDefL [("category", "LC_TIME"), ("locale", "")] args of
        Right [gCat, gName] ->
            case (TG.asText gCat, unpack . TG.asText $ gName) of
                -- TODO: disable until we've upgraded Polysemy and can add the
                -- Effect.Locale to the bundle that will be written for the
                -- RunSem r
                {-("LC_TIME", "") -> TG.toGVal <$> getLocale Nothing-}
                {-("LC_TIME", localeName) -> TG.toGVal <$> getLocale (Just localeName)-}
                (cat, localeName) -> return def -- valid call, but category not implemented
        _ -> do
            TG.liftRun $ CP.log @String "'getlocale' requirs a string category and name - invalid args"
            pure def
