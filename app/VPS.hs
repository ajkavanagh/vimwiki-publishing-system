module VPS where

import           System.Environment (getArgs)

import           Control.Monad      (when)

import           Lib.Utils          (debugArgs, isDebug)
import           SiteGen            (sitegenProgram)


main :: IO ()
main = do
    debug <- isDebug
    when debug debugArgs
    sitegenProgram
