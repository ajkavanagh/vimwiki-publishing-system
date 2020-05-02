{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}


module Lib.PandocUtilis where



-- for Pandoc processing
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.Text.Titlecase    (titlecase)
import qualified Qq                     as Q
import qualified Text.Pandoc            as TP
import qualified Text.Pandoc.Builder    as B
import qualified Text.Pandoc.Definition as TPD
import qualified Text.Pandoc.Error      as TPE
import qualified Text.Pandoc.Walk       as TPW
import qualified Text.Regex.PCRE.Heavy  as PCRE


-- | re-write Pandoc Links if they map to a source name.  i.e. map it to a
-- route; if a link is removed, then the text needs to be just but back into the
-- list.  However, that is [Inline] and it replaces a Inline.  So we have to
-- process the list and give back a list.
--
-- The Link, if it's local, will be the filename minus the extension.  i.e. we
-- have to add the extension and then try to match it to the filepath

processPandocLinks
    :: HashMap.HashMap FilePath H.SourcePageHeader
    -> TP.Pandoc
    -> TP.Pandoc
processPandocLinks hmap = TPW.walk (walkLinksInInlines hmap)

walkLinksInInlines
    :: HashMap.HashMap FilePath H.SourcePageHeader
    -> [TP.Inline]
    -> [TP.Inline]
walkLinksInInlines hmap xs = L.concat $ walkLinksInInlines' hmap [] xs


walkLinksInInlines
    :: HashMap.HashMap FilePath H.SourcePageHeader
    -> [[TP.Inline]]
    -> [TP.Inline]
    -> [[TP.Inline]]
walkLinksInInlines hmap ds xs =
    let (as, bs) = L.break findLink xs
     in case bs of
         [] -> L.reverse $ as : ds
         (b:bs') -> walkLinksInInlines' hmap ((maybeRewriteLink hmap b) : ds) bs'
  where
      findLink (TPD.Link _ _ _) = True
      findLink _ = False


-- rewrite the Link.  Options
--  1. if it isn't local then just leave it alone.
--  2. If it is local and we find it in the hashmap then re-write to the hashmap
--     slug (plus any #part as part of the target)
--  3. If it is local but we couldn't match it, then we need to remove the link
--     and substitute in the text or inlines.  Most likely the text.
maybeRewriteLink
    :: HashMap.HashMap FilePath H.SourcePageHeader
    -> TP.Inline
    -> [TP.Inline]
maybeRewriteLink hmap (TPD.Link attr desc (url, title)) = undefined
