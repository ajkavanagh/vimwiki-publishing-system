module Lib.Dates (parseDate) where


import           Control.Applicative.Combinators (choice)
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)


{--|
    This module parses date/times in as many ways as possible
-}

dateTimeFormats = [ "%Y-%m-%dT%M:%S%Q%z"
                  , "%Y-%m-%d %M:%S%Q%z"
                  , "%Y-%m-%dT%M:%S%Q"
                  , "%Y-%m-%d %M:%S%Q"
                  , "%Y-%m-%d %M:%S"
                  , "%Y-%m-%d"
                  , "%d-%m-%Y %M:%S%Q%z"
                  , "%d-%m-%y %M:%S%Q%z"
                  , "%d-%m-%Y %M:%S%Q"
                  , "%d-%m-%y %M:%S%Q"
                  , "%d-%m-%Y %M:%S"
                  , "%d-%m-%y %M:%S"
                  , "%d-%m-%Y"
                  , "%d-%m-%y"
                  , "%d/%m/%Y %M:%S%Q%z"
                  , "%d/%m/%y %M:%S%Q%z"
                  , "%d/%m/%Y %M:%S%Q"
                  , "%d/%m/%y %M:%S%Q"
                  , "%d/%m/%Y %M:%S"
                  , "%d/%m/%y %M:%S"
                  , "%d/%m/%Y"
                  , "%d/%m/%y"
                  , "%b %d, %y"
                  , "%b %d, %Y"]

parseDate :: String -> Maybe UTCTime
parseDate inp = choice $ map parser dateTimeFormats
  where
      parser fmt = parseTimeM True defaultTimeLocale fmt inp
