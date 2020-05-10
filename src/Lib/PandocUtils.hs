{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}


module Lib.PandocUtils where



-- for Pandoc processing
import qualified Data.List              as L
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Data.HashMap.Strict    (HashMap)
import qualified Data.HashMap.Strict    as HashMap
import qualified Text.Pandoc            as TP
import qualified Text.Pandoc.Builder    as B
import qualified Text.Pandoc.Definition as TPD
import qualified Text.Pandoc.Error      as TPE
import qualified Text.Pandoc.Walk       as TPW

import qualified Network.URI            as NU
import qualified Lib.Header             as H

import qualified Lib.SiteGenState       as SGS


-- | re-write Pandoc Links if they map to a source name.  i.e. map it to a
-- route; if a link is removed, then the text needs to be just but back into the
-- list.  However, that is [Inline] and it replaces a Inline.  So we have to
-- process the list and give back a list.
--
-- The Link, if it's local, will be the filename minus the extension.  i.e. we
-- have to add the extension and then try to match it to the filepath

processPandocLinks :: SGS.VimWikiLinkToSPH -> TP.Pandoc -> TP.Pandoc
processPandocLinks hmap = TPW.walk (walkLinksInInlines hmap)

walkLinksInInlines :: SGS.VimWikiLinkToSPH -> [TP.Inline] -> [TP.Inline]
walkLinksInInlines hmap xs = L.concat $ walkLinksInInlines' hmap [] xs


walkLinksInInlines' :: SGS.VimWikiLinkToSPH -> [[TP.Inline]] -> [TP.Inline]
                    -> [[TP.Inline]]
walkLinksInInlines' hmap ds xs =
    let (as, bs) = L.break findLink xs
     in case bs of
         [] -> L.reverse $ as : ds
         (b:bs') -> walkLinksInInlines' hmap (maybeRewriteLink hmap b : as : ds) bs'
  where
      findLink TPD.Link {} = True
      findLink _ = False


-- rewrite the Link.  Options
--  1. if it isn't local then just leave it alone.
--  2. If it is local and we find it in the hashmap then re-write to the hashmap
--     slug (plus any #part as part of the target)
--  3. If it is local but we couldn't match it, then we need to remove the link
--     and substitute in the text or inlines.  Most likely the text.
-- use Network.URI to detect the whether it is relative or a URI.  If it is
-- relative, parse it, pull out the relative bit, and match it against the
-- relative link of the the SourcePageHeader items.
maybeRewriteLink :: SGS.VimWikiLinkToSPH -> TP.Inline -> [TP.Inline]
maybeRewriteLink hmap link@(TPD.Link attr desc (url, title))
  -- it's a relative reference; they should ALL be within the site
  | NU.isRelativeReference url =
      -- can we parse it
      case NU.parseRelativeReference url of
          Nothing -> [link]   -- assume it's something else and leave it alone
          Just uri -> case HashMap.lookup (NU.uriPath uri) hmap of
              -- if we don't find it, then convert it to text
              Nothing -> if null desc
                           then B.toList $ B.text title
                           else desc
              -- otherwise re-write it to the route; note we need to add back in
              -- any of the other bits of the url (query and fragment)
              Just sph ->
                  let newUri = show (uri {NU.uriPath=H.phRoute sph})
                   in [TPD.Link attr desc (newUri, title)]
  -- it was absolute or something else; thus we just ignore the link
  | otherwise = [link]
maybeRewriteLink _ _ = error "Must only pass a Link to this function"
