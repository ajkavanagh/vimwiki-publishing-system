module Lib.Dates (parseDate) where


import           Control.Applicative.Combinators (choice)
import           Data.Time.Clock                 (UTCTime)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)


{--|
    This module parses date/times in as many ways as possible
-}

dateTimeFormats = [ "%Y-%m-%dT%H:%M:%S%Q%Z"
                  , "%Y-%m-%dT%H:%M:%S%Q%EZ"
                  , "%Y-%m-%d %H:%M:%S%Q%Z"
                  , "%Y-%m-%d %H:%M:%S%Q%EZ"
                  , "%Y-%m-%dT%H:%M:%S%Q"
                  , "%Y-%m-%d %H:%M:%S%Q"
                  , "%Y-%m-%d %H:%M:%S"
                  , "%Y-%m-%d %H:%M"
                  , "%Y-%m-%d"
                  , "%d-%m-%Y %H:%M:%S%Q%Z"
                  , "%d-%m-%Y %H:%M:%S%Q%EZ"
                  , "%d-%m-%y %H:%M:%S%Q%Z"
                  , "%d-%m-%y %H:%M:%S%Q%EZ"
                  , "%d-%m-%Y %H:%M:%S%Q"
                  , "%d-%m-%y %H:%M:%S%Q"
                  , "%d-%m-%Y %H:%M:%S"
                  , "%d-%m-%y %H:%M:%S"
                  , "%d-%m-%Y %H:%M"
                  , "%d-%m-%y %H:%M"
                  , "%d-%m-%Y"
                  , "%d-%m-%y"
                  , "%d/%m/%Y %H:%M:%S%Q%Z"
                  , "%d/%m/%Y %H:%M:%S%Q%EZ"
                  , "%d/%m/%y %H:%M:%S%Q%Z"
                  , "%d/%m/%y %H:%M:%S%Q%EZ"
                  , "%d/%m/%Y %H:%M:%S%Q"
                  , "%d/%m/%y %H:%M:%S%Q"
                  , "%d/%m/%Y %H:%M:%S"
                  , "%d/%m/%y %H:%M:%S"
                  , "%d/%m/%Y %H:%M"
                  , "%d/%m/%y %H:%M"
                  , "%d/%m/%Y"
                  , "%d/%m/%y"
                  , "%b %d, %y"
                  , "%b %d, %Y"]

parseDate :: String -> Maybe UTCTime
parseDate inp = choice $ map parser dateTimeFormats
  where
      parser fmt = parseTimeM True defaultTimeLocale fmt inp
