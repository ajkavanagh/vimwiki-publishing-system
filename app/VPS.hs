module VPS where

import System.Environment (getProgName, getArgs)

import Control.Monad (when)

import LibVps (vw2html)
import SiteGen (sitegenProgram)
import Lib (debugArgs, isDebug)

{- based on the program name, we have to pick between two runtimes.

1. The first runtime is called vw2html which is called from Vimwiki2HMTL in vim.
2. The second is  when the program is called sitegen. This does static site
   generation.  This module chooses between VPS and SiteGen run times based on
   program name.

-}

main :: IO ()
main = do
    name <- getProgName
    debug <- isDebug
    when debug debugArgs
    if name == "vw2html"
      then vw2html
      else sitegenProgram
