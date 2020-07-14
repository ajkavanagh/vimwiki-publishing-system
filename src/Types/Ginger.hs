module Types.Ginger where

import           TextShow

import           Data.Text (Text)


newtype GingerException = GingerException Text

instance Show GingerException where
    show ex = "Ginger Exception issue: " ++ ss
      where
          ss = case ex of
              (GingerException s)   -> show s

