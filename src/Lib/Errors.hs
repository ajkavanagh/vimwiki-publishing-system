module Lib.Errors
    where


import           Effect.File       (FileException)
import           Effect.Ginger     (GingerException)
import           Lib.SiteGenConfig (ConfigException)



data SiteGenException
    = FileExceptionWrapped FileException
    | GingerExceptionWrapped GingerException
    | ConfigExceptionWrapped ConfigException
--  | PandocExceptionWrapped PandocException
    deriving Show


