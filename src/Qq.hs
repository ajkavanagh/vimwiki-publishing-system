{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Qq
    {-( someFunc-}
    {-, someOtherFunc-}
    {-, runPanDoc-}
    {-)-}
      where


import qualified Text.Regex.PCRE.Heavy as PCRE
import qualified Text.Regex.PCRE.Light as PCREL

re = PCRE.mkRegexQQ [PCREL.utf8, PCREL.ungreedy]
