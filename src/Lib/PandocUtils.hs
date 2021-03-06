{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}


module Lib.PandocUtils
    ( processPandocLinks
    , convertVimWikiLinks
    , countWords
    , findSummary
    , takeNWords
    , flattenPandoc
    , extractStrSpace
    , getSummaryPlain
    , getSummaryPandoc
    , stripMoreIndicator
    , getSummaryNPlain
    , getSummaryNPandoc
    , TocItem(..)
    , extractToc
    , dumpToc
    , loadTocEither
    , buildPandocFromTocItems
    , parseMarkdown
    , pandocToContentTextEither
    , pandocToSummaryTextEither
    , extractTocItemsToByteString
    , renderTocItemsToHtml
    -- testing
    , testToc
    , runTest
    , runTest2
    ) where

import           TextShow

-- General
import           Control.Monad            (liftM)

-- for Pandoc processing
import qualified Data.ByteString          as BS
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.List                as L
import           Data.Maybe               (isJust)
import           Data.Monoid              (Sum (..), getSum)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8')

import           Data.DList               (DList)
import qualified Data.DList               as DList

import qualified Text.Pandoc              as TP
import qualified Text.Pandoc.Builder      as B
import qualified Text.Pandoc.Definition   as TPD
import qualified Text.Pandoc.Error        as TPE
import qualified Text.Pandoc.Highlighting as TPH
import qualified Text.Pandoc.Walk         as TPW

import qualified Network.URI              as NU

-- to handle parsing out vimwiki [[text|link]] style links into Links
import           Text.Parsec              (parse, try)
import           Text.Parsec.Char         (anyChar, char)
import           Text.Parsec.Combinator   (many1, manyTill, notFollowedBy)
import           Text.Parsec.Text         (Parser)

import           Control.Applicative      ((<|>))

-- for persisting the table of contents as a blob -- we need this for stashing
-- it and grabbing it as needed
import           Data.Yaml                ((.!=), (.:), (.:?), (.=))
import qualified Data.Yaml                as Y

import           Types.Errors             as TE
import qualified Types.SiteGenState       as SGS

import qualified Lib.Header               as H
import           Lib.Utils                (strToLower)

-- testing - remove
import           Data.Either              (fromRight)


-- Provide the set of options to use with the reader; eventually we'll allow
-- this to be override at the program creation stage.
pandocMarkdownArgs :: TP.ReaderOptions
pandocMarkdownArgs = TP.def { TP.readerExtensions =
    TP.extensionsFromList
        -- uris
        [ TP.Ext_autolink_bare_uris   -- bare http(s):// converts to links
        -- code blocks
        , TP.Ext_backtick_code_blocks -- enable ``` for code blocks
        , TP.Ext_inline_code_attributes     -- allow `<$>`{.haskell} inline
        -- block formatting
        , TP.Ext_escaped_line_breaks  -- allow \<cr> to be a hard break
        , TP.Ext_blank_before_header  -- require a blank line before # header
        , TP.Ext_space_in_atx_header -- requires a space after #
        , TP.Ext_auto_identifiers    -- spec identifiers for headings
        , TP.Ext_header_attributes -- allow {#identifier ...} after heading
        , TP.Ext_implicit_header_references  -- allow [That Heading] links
        , TP.Ext_blank_before_blockquote  -- ensure a blank before a block quotation
        , TP.Ext_fenced_code_attributes -- allow attributes on code blocks
        , TP.Ext_line_blocks  -- allow | for line blocks.
        , TP.Ext_fancy_lists    -- enable lots of different list types
        , TP.Ext_startnum  -- enable starting of lists at a number e.g. 9)
        , TP.Ext_task_lists     -- use - [x] for task lists
        , TP.Ext_definition_lists   -- allow markdown definition lists
        , TP.Ext_example_lists    -- enable use of (@) list numbering
        -- tables
        , TP.Ext_table_captions   -- enable use of Table: this is the caption
        , TP.Ext_pipe_tables     -- use '|' to indicate columns
        -- formatting
        , TP.Ext_all_symbols_escapable  -- allow escaping of */_ etc.
        , TP.Ext_intraword_underscores   -- allow _ in words, not as formatting
        , TP.Ext_strikeout                  -- allow use of ~~struckout~~ formatting
        , TP.Ext_superscript                -- allow use of ^ e.g. 2^2^
        , TP.Ext_subscript                  -- allow use of H~2~O
        -- math
        , TP.Ext_tex_math_dollars           -- recognise $some text math expression$
        -- raw html attribute
        , TP.Ext_raw_attribute              -- allow use of `<some thing/>`{=html}
        -- processing for filters
        , TP.Ext_native_divs                -- turns divs in Pandoc AST Div elements
        , TP.Ext_native_spans               -- turns spans into Pandoc AST Span elements
        -- images
        , TP.Ext_implicit_figures           -- allow captioned images in bare paragraphs
        , TP.Ext_link_attributes            -- switch on attributes for links
        -- syntax for divs and spans
        , TP.Ext_fenced_divs                -- use ::: as a fenced div
        , TP.Ext_bracketed_spans            -- allow [some text]{.class ...} to be a Span
        -- footnotes and inline notes
        , TP.Ext_footnotes                  -- switch on footnotes [^1]
        , TP.Ext_inline_notes               -- enable Some.^[inline note]
        ]
    }


-- Provide the set of options to use with the writer to HTML.  This only needs
-- to provide the fragment as we aren't doing complex templates of any kind.  We
-- aren't even wrapping the content in a div; that'll go in the Ginger template.
pandocHtmlArgs ::  Maybe T.Text -> TP.WriterOptions
pandocHtmlArgs mStyle =
    let sStyle = flip L.lookup TPH.highlightingStyles =<< mStyle
     in TP.def { TP.writerTemplate = Nothing
               , TP.writerTableOfContents = False
                        --, TP.writerHTMLMathMethod = TP.MathJax "some url"
               , TP.writerSectionDivs = True
               , TP.writerHighlightStyle = sStyle
               , TP.writerHtmlQTags = True
               }

pandocPlainArgs :: TP.WriterOptions
pandocPlainArgs = TP.def


-- | parse a markdown document all the way to html.
-- Firstly we have to get the initial AST.  Then we munge the AST for
--  - links
--  - summary (extract that bit)
--  - table of contents (extract and store to JSON)

parseMarkdown :: BS.ByteString -> Either TE.SiteGenError TP.Pandoc
parseMarkdown bs =
    case decodeUtf8' bs of
        Left e -> Left $ TE.PageDecodeError (showt e)
        Right txt ->
            let result = TP.runPure $ TP.readMarkdown pandocMarkdownArgs txt
             in case result of
                Left e    -> Left $ TE.PandocReadError e
                Right ast -> Right ast


-- | process the pandoc AST so that:
--  * wikilink links are discovered and turned into Pandoc AST links
--  * local links that don't point to any pages are removed.
--  * links are 'fixed up' so they point to the right place in the eventual
--    site.
processPandocAST :: SGS.VimWikiLinkToSM -> TP.Pandoc -> TP.Pandoc
processPandocAST hmap ast = processPandocLinks hmap $ convertVimWikiLinks ast


-- take the Pandoc AST (that's probably been processed) and process it to HTML
-- for content.  Then return this content.
pandocToContentTextEither :: Maybe T.Text -> TP.Pandoc -> Either TE.SiteGenError T.Text
pandocToContentTextEither mStyle ast =
    let result = TP.runPure $ TP.writeHtml5String (pandocHtmlArgs mStyle) ast
     in case result of
        Left e    -> Left $ TE.PandocWriteError e
        Right txt -> Right txt


-- take the PandocAST (that'll have been processed) and extract the summary.
-- Use the SiteGenConfig reader to extract the ProgramDefaults so that we can
-- use the 'extract' N words as needed.
pandocToSummaryTextEither
    :: Maybe T.Text     -- ^ The text style for skylighting to use
    -> Int              -- ^ the number of words to use
    -> TP.Pandoc        -- ^ the Pandoc document to fetch the summary from
    -- Plain HTML and marked up HTML versions of the summary
    -> Either TE.SiteGenError ((T.Text, Bool), (T.Text, Bool))
pandocToSummaryTextEither mStyle n ast = do
    plain <- renderWithOneOfEither (TP.writePlain pandocPlainArgs)
                                   getSummaryPlain
                                   (getSummaryNPlain n)
                                   ast
    rich <- renderWithOneOfEither (TP.writeHtml5String (pandocHtmlArgs mStyle))
                                  getSummaryPandoc
                                  (getSummaryNPandoc n)
                                  ast
    pure (plain, rich)


-- | helper to choose one of the summary functions
renderWithOneOfEither
    :: (TP.Pandoc -> TP.PandocPure T.Text)
    -> (TP.Pandoc -> Maybe TP.Pandoc)
    -> (TP.Pandoc -> (TP.Pandoc, Bool))
    -> TP.Pandoc
    -> Either TE.SiteGenError (T.Text, Bool) -- ^ return text and truncated flag
renderWithOneOfEither pandocF f1 f2 ast =
    let mAst1 = f1 ast
        (ast2, truncated) = f2 ast
        mAst = mAst1 <|> pure ast2
        truncated' = truncated || isJust mAst1
     in case mAst of
        Nothing -> Left $ TE.PandocProcessError "Couldn't extract text?"
        Just ast' ->
            let resultTxt = TP.runPure $ pandocF ast'
             in case resultTxt of
                Left e    -> Left $ TE.PandocWriteError e
                Right txt -> Right (txt, truncated')


-- | re-write Pandoc Links if they map to a source name.  i.e. map it to a
-- route; if a link is removed, then the text needs to just be put but back into
-- the list.  However, that is [Inline] and it replaces a Inline.  So we have to
-- process the list and give back a list.
--
-- The Link, if it's local, will be the filename minus the extension.  i.e. we
-- have to add the extension and then try to match it to the filepath

processPandocLinks :: SGS.VimWikiLinkToSM -> TP.Pandoc -> TP.Pandoc
processPandocLinks hmap = TPW.walk (walkLinksInInlines hmap)


walkLinksInInlines :: SGS.VimWikiLinkToSM -> [TP.Inline] -> [TP.Inline]
walkLinksInInlines hmap xs = L.concat $ walkLinksInInlines' hmap [] xs


walkLinksInInlines' :: SGS.VimWikiLinkToSM
                    -> [[TP.Inline]]
                    -> [TP.Inline]
                    -> [[TP.Inline]]
walkLinksInInlines' hmap ds xs =
    let (as, bs) = L.break findLink xs
     in case bs of
         [] -> L.reverse $ as : ds
         (b:bs') -> walkLinksInInlines' hmap (maybeRewriteLink hmap b : as : ds) bs'
  where
      findLink TPD.Link {} = True
      findLink _           = False


-- rewrite the Link.  Options
--  1. if it isn't local then just leave it alone.
--  2. If it is local and we find it in the hashmap then re-write to the hashmap
--     slug (plus any #part as part of the target)
--  3. If it is local but we couldn't match it, then we need to remove the link
--     and substitute in the text or inlines.  Most likely the text.
-- use Network.URI to detect the whether it is relative or a URI.  If it is
-- relative, parse it, pull out the relative bit, and match it against the
-- relative link of the the SourceMetadata items.
maybeRewriteLink :: SGS.VimWikiLinkToSM -> TP.Inline -> [TP.Inline]
maybeRewriteLink hmap link@(TPD.Link attr desc (url, title)) =
    let url' = T.unpack url
  -- it's a relative reference; they should ALL be within the site
     in if NU.isRelativeReference url'
          -- can we parse it
          then case NU.parseRelativeReference url' of
            Nothing -> [link]   -- assume it's something else and leave it alone
            Just uri -> case HashMap.lookup (strToLower $ NU.uriPath uri) hmap of
                -- if we don't find it, then convert it to text
                Nothing -> if null desc
                            then B.toList $ B.text title
                            else desc
                -- otherwise re-write it to the route (or permalink override);
                -- note we need to add back in any of the other bits of the url
                -- (query and fragment)
                Just sm ->
                    let newUri = show (uri {NU.uriPath=H.resolveLinkFor sm})
                    in [TPD.Link attr desc (T.pack newUri, title)]
          -- it was absolute or something else; thus we just ignore the link
          else [link]
maybeRewriteLink _ _ = error "Must only pass a Link to this function"


----

-- | Process Block level items in the Pandoc AST to look for Str/Space segments
-- that might contain vimwiki links.  We have to process the Block level items,
-- as we need to only to Para, Plain, DefinitionList (for the Inlines), Header
-- and Table [Inline] blocks.  This is so we ONLY process the 'str' segements
-- that make sense.
--
-- The [Inline] segments are further complicated as we are interested in
-- Str,Space,Str,Space, segments; but also have to look inside the other
-- Inline section's [Inline] blocks for the same.  i.e. it's recursive.
--
-- So it's quite a bit of construction, de-construction and parsing to convert
-- them into links.  This will need a few tests when it's turned into a library.

-- NOTE: this is using the version of Pandoc that runs with Strings.  When
-- updated to Pandoc version that uses Text the T.unpack and T.pack will be
-- removed.


convertVimWikiLinks :: TP.Pandoc -> TP.Pandoc
convertVimWikiLinks = TPW.walk processBlockVWL


processBlockVWL :: TP.Block -> TP.Block
processBlockVWL (TP.Plain ls) = TP.Plain $ processInlines ls
processBlockVWL (TP.Para ls) = TP.Para $ processInlines ls
processBlockVWL (TP.LineBlock lls) = TP.LineBlock $ map processInlines lls
processBlockVWL (TP.DefinitionList dl) = TP.DefinitionList $ map doDefn dl
  where
      doDefn :: ([TP.Inline], [[TP.Block]]) -> ([TP.Inline], [[TP.Block]])
      doDefn (ls, bls) = (processInlines ls, bls)
processBlockVWL (TP.Header int attr ls) = TP.Header int attr $ processInlines ls
processBlockVWL (TP.Table ls a b c d) = TP.Table (processInlines ls) a b c d
-- finally just ignore the rest
processBlockVWL x = x


-- search through the inlines for Str,Space ... sequences, join them, parse
-- them, convert them back to inlines.  i.e. we are trying to find the links in
-- amongst all the processed AST whilst keeping the AST intact.
processInlines :: [TP.Inline] -> [TP.Inline]
processInlines [] = []
processInlines ys@(x:xs) = case x of
    (TPD.Str text)       -> buildStrSequence [] ys
    (TPD.Emph ls)        -> processEmbedded1 TPD.Emph ls xs
    (TPD.Strong ls)      -> processEmbedded1 TPD.Strong ls xs
    (TPD.Strikeout ls)   -> processEmbedded1 TPD.Strikeout ls xs
    (TPD.Superscript ls) -> processEmbedded1 TPD.Superscript ls xs
    (TPD.Subscript ls)   -> processEmbedded1 TPD.Subscript ls xs
    (TPD.SmallCaps ls)   -> processEmbedded1 TPD.SmallCaps ls xs
    (TPD.Quoted qt ls)   -> processEmbedded1 (TPD.Quoted qt) ls xs
    (TPD.Cite cite ls)   -> processEmbedded1 (TPD.Cite cite) ls xs
    -- everything else is just passed through and ignored.
    _                    -> x : processInlines xs


-- | process the inlines inside the Inline constructor, and then continue with the
-- rest.
processEmbedded1 :: ([TP.Inline] -> TP.Inline) -> [TP.Inline] -> [TP.Inline] -> [TP.Inline]
processEmbedded1 inlineCons ls ys = inlineCons (processInlines ls) : processInlines ys


-- | build up a Str,Space,Str... sequence and then call the processStrSequence on
-- it.  The first non Str or Space is then handed back to processInLines
buildStrSequence :: [TP.Inline] -> [TP.Inline] -> [TP.Inline]
buildStrSequence xs (y@(TPD.Str _):ys) = buildStrSequence (xs ++ [y]) ys
buildStrSequence xs (y@TPD.Space:ys) = buildStrSequence (xs ++ [y]) ys
buildStrSequence xs ys = processStrSequence xs ++ processInlines ys


-- | now build a string out of the Str+Space sequence and parse it, reconstruct
--it and return it as a sequence of inlines
processStrSequence :: [TP.Inline] -> [TP.Inline]
processStrSequence xs = concatMap toInline $ parseToLinkPart $ T.concat $ map toStr xs


toStr :: TP.Inline -> T.Text
toStr (TPD.Str txt) = txt
toStr TPD.Space     = " "
toStr _             = ""


-- | convert LinkPart pieces back into a sequence of Inline elements.  Uses the
-- Text.Pandoc.Builder 'text' function to convert text sequences.
toInline :: LinkPart -> [TP.Inline]
toInline (RegularText text) = B.toList $ B.text text
toInline (Link link desc) =
    [TP.Link TPD.nullAttr (B.toList $ B.text desc) (link, desc)]


-- | Parse a string and find any vimwiki style links as LinkPart

data LinkPart = RegularText T.Text
              | Link T.Text T.Text
              deriving Show


parseToLinkPart :: T.Text -> [LinkPart]
parseToLinkPart txt =
    case parse matchManyWikiLinks "" txt of
        Right lps -> concat lps
        Left _    -> [RegularText txt]


matchStartLink :: Parser ()
matchStartLink = char '[' >> char '[' >> pure ()


matchEndLink :: Parser ()
matchEndLink = char ']' >> char ']' >> pure ()


matchSepInLink :: Parser ()
matchSepInLink = char '|' >> pure ()


matchAllChars :: Parser [LinkPart]
matchAllChars = do
    s <- many1 anyChar
    pure [RegularText $ T.pack s]


matchPureLinkText :: Parser Char
matchPureLinkText = do
    c <- anyChar
    notFollowedBy (char '|')
    pure c

matchLink :: Parser [LinkPart]
matchLink = do
    before <- manyTill anyChar (try matchStartLink)
    linkText <- manyTill matchPureLinkText (try matchEndLink)
    pure [RegularText (T.pack before), Link (T.pack linkText) (T.pack linkText)]

matchLink2 :: Parser [LinkPart]
matchLink2 = do
    before <- manyTill anyChar (try matchStartLink)
    linkText <- manyTill anyChar (try matchSepInLink)
    desc <- manyTill anyChar (try matchEndLink)
    pure [RegularText (T.pack before),
          Link (T.pack linkText) (T.pack desc)]


matchWikiLink :: Parser [LinkPart]
matchWikiLink = try matchLink <|> try matchLink2 <|> matchAllChars


matchManyWikiLinks :: Parser [[LinkPart]]
matchManyWikiLinks = many1 matchWikiLink

---

-- Find summary; two techniques; one looks for <!--more--> as a text string in a
-- para/plain, the other takes the first N words.  In either case, the headings
-- then need to be removed and just the paras remain (if it's rich) or just the
-- text if it's plain

-- find the summary string <!--more--> in the text (exactly as written).
--
-- It will be in a Para or Plain at the top level; if it's not (i.e. we can't
-- find it) then return Nothing.  In the Plain or Para we are only looking at
-- the top level Str elements for the complete string and we return everything
-- before that string.  i.e. not in emp or links, or anywhere else.
--
-- So the steps are:
--  1. try for the <!--more--> or take N words into a new Pandoc
--  2. Flatten the Pandoc without headers and just plain/para blocks.
--  3a. For rich, return that (it may includes links, etc.)
--  3b. For plain, flatten that to just Text.
--
--  In each of these functions, the return value is the Maybe(Pandoc, Bool)
--  where the Bool is whether the summary is actually a trucation of the
--  document.

-- If this returns True, then it is a truncated view of the document
getSummaryPlain :: TP.Pandoc -> Maybe TP.Pandoc
getSummaryPlain pd = extractStrSpace . flattenPandoc <$> findSummary pd

-- The boolean is whether it is trucated
getSummaryNPlain :: Int -> TP.Pandoc -> (TP.Pandoc, Bool)
getSummaryNPlain n pd =
    let (words, truncated) = takeNWords n pd
     in (extractStrSpace $ flattenPandoc words, truncated)

-- If this returns True, then it is a truncated view of the document
getSummaryPandoc :: TP.Pandoc -> Maybe TP.Pandoc
getSummaryPandoc pd = flattenPandoc <$> findSummary pd

-- The boolean is whether it is trucated
getSummaryNPandoc :: Int -> TP.Pandoc -> (TP.Pandoc, Bool)
getSummaryNPandoc n pd =
    let (words, truncated) = takeNWords n pd
     in (flattenPandoc words, truncated)


-- | flatten the Pandoc by removing every block level item except Para or Plain
flattenPandoc :: TP.Pandoc -> TP.Pandoc
flattenPandoc (TP.Pandoc meta bs) = TP.Pandoc meta (filter isPlainOrPara bs)


isPlainOrPara :: TP.Block -> Bool
isPlainOrPara (TP.Plain _) = True
isPlainOrPara (TP.Para _)  = True
isPlainOrPara _            = False


-- | extract just the Str, Space elements from the Pandoc provided in Plain or
-- Para elements.  Flatten the Emp, Super, Strikeout, etc. elements down so we
-- get a single list of Str/Space and then convert them into a single TP.Para
-- inside the Pandoc

extractStrSpace :: TP.Pandoc -> TP.Pandoc
extractStrSpace (TP.Pandoc meta bs) =
    TP.Pandoc meta [TP.Para $ L.intercalate [TP.Space] (map extractBlock bs)]


extractBlock :: TP.Block -> [TP.Inline]
extractBlock (TP.Plain is) = concatMap extractInlines is
extractBlock (TP.Para is)  = concatMap extractInlines is
extractBlock _             = []


extractInlines :: TP.Inline -> [TP.Inline]
extractInlines TP.Space = [TP.Space]
extractInlines i@(TP.Str _) = [i]
extractInlines i =
    let ls = (case i of
            TP.Emph is        -> is
            TP.Strong is      -> is
            TP.Strikeout is   -> is
            TP.Superscript is -> is
            TP.Subscript is   -> is
            TP.SmallCaps is   -> is
            TP.Link _ is _    -> is
            TP.Span _ is      -> is
            _                 -> [])
    in concatMap extractInlines ls



findSummary :: TP.Pandoc -> Maybe TP.Pandoc
findSummary (TP.Pandoc meta bs) = findSummary' meta [] bs

findSummary' :: TP.Meta -> [TP.Block] -> [TP.Block] -> Maybe TP.Pandoc
-- if we ran out of blocks, then we never found the <!--more--> string
findSummary' meta _ [] = Nothing
findSummary' meta ds (b@(TP.Plain p):bs) =
    findSummaryInline meta ds TP.Plain bs [] p
findSummary' meta ds (b@(TP.Para p):bs) =
    findSummaryInline meta ds TP.Para bs [] p
findSummary' meta ds (b:bs) = findSummary' meta (b:ds) bs


findSummaryInline :: TP.Meta         -- meta we need to reconstruct
                  -> [TP.Block]      -- done blocks we need to keep
                  -> ([TP.Inline] -> TP.Block) -- constructor to build block
                  -> [TP.Block]      -- blocks yet to be processed
                  -> [TP.Inline]     -- inlines already processed
                  -> [TP.Inline]     -- inlines to process
                  -> Maybe TP.Pandoc -- return value
-- if we have no more inlines to look at, put them back and carry on searching
-- the next block
findSummaryInline meta dbs cons bs dis [] = findSummary' meta (cons (reverse dis):dbs) bs
-- if we have an inline with a Str element, we have to see if it is <!--more-->
findSummaryInline meta dbs cons bs dis (TP.Str s:ils)
    | s == "<!--more-->" = Just $ TP.Pandoc meta (reverse (cons (reverse dis):dbs))
findSummaryInline meta dbs cons bs dis (i:ils) =
    findSummaryInline meta dbs cons bs (i:dis) ils


-- Finally we need to be able to strip the <!--more--> item out of the document if
-- it exists.  This will remove any <!--more--> text anywhere in the document.
-- if you need to keep it, then put it in a code link.
--
stripMoreIndicator :: TP.Pandoc -> TP.Pandoc
stripMoreIndicator = TPW.walk stripMoreIndicator'


stripMoreIndicator' :: TP.Inline -> TP.Inline
stripMoreIndicator' i@(TP.Str s) = if s == "<!--more-->" then TP.Str "" else i
stripMoreIndicator' x            = x

---

-- Count the words in a Pandoc document; used to calculate reading time for a
-- document.
countWords :: TP.Pandoc -> Int
countWords = getSum . TPW.query countWordsInBlock . stripMoreIndicator

countWordsInBlock :: TP.Block -> Sum Int
countWordsInBlock (TP.Plain xs)          = countWordsInInlines xs
countWordsInBlock (TP.Para xs)           = countWordsInInlines xs
countWordsInBlock (TP.LineBlock xxs)     = mconcat $ map countWordsInInlines xxs
countWordsInBlock (TP.CodeBlock _ txt)   = countWordsInText txt
countWordsInBlock (TP.RawBlock _ txt)    = countWordsInText txt
countWordsInBlock (TP.BlockQuote bs)     = countWordsInBlocks bs
countWordsInBlock (TP.OrderedList _ bbs) = mconcat $ map countWordsInBlocks bbs
countWordsInBlock (TP.BulletList bbs)    = mconcat $ map countWordsInBlocks bbs
countWordsInBlock (TP.DefinitionList ds) = mconcat $ map processDL ds
  where
      processDL :: ([TP.Inline], [[TP.Block]]) -> Sum Int
      processDL (xs, bbs) = countWordsInInlines xs <> mconcat (map countWordsInBlocks bbs)
countWordsInBlock (TP.Header _ _ xs)     = countWordsInInlines xs
countWordsInBlock (TP.Table xs _ _ bbs bbbs)
  =  countWordsInInlines xs
  <> mconcat (map countWordsInBlocks bbs)
  <> mconcat (map (mconcat . map countWordsInBlocks) bbbs)
countWordsInBlock (TP.Div _ bs)          = countWordsInBlocks bs
countWordsInBlock _                      = Sum 0


countWordsInBlocks :: [TP.Block] -> Sum Int
countWordsInBlocks = mconcat . map countWordsInBlock


countWordsInInlines :: [TP.Inline] -> Sum Int
countWordsInInlines = mconcat . map countWordsInInline


countWordsInInline :: TP.Inline -> Sum Int
countWordsInInline (TP.Str txt)            = countWordsInText txt
countWordsInInline (TP.Emph xs)            = countWordsInInlines xs
countWordsInInline (TP.Strong xs)          = countWordsInInlines xs
countWordsInInline (TP.Strikeout xs)       = countWordsInInlines xs
countWordsInInline (TP.Superscript xs)     = countWordsInInlines xs
countWordsInInline (TP.Subscript xs)       = countWordsInInlines xs
countWordsInInline (TP.SmallCaps xs)       = countWordsInInlines xs
countWordsInInline (TP.Quoted _ xs)        = countWordsInInlines xs
countWordsInInline (TP.Cite _ xs)          = countWordsInInlines xs
countWordsInInline (TP.Code _ txt)         = countWordsInText txt
countWordsInInline (TP.RawInline _ txt)    = countWordsInText txt
countWordsInInline (TP.Link _ _ (_, txt))  = countWordsInText txt
countWordsInInline (TP.Image _ _ (_, txt)) = countWordsInText txt
countWordsInInline (TP.Note bs)            = mconcat $ map countWordsInBlock bs
countWordsInInline (TP.Span _ xs)          = countWordsInInlines xs
countWordsInInline _                       = Sum 0


countWordsInText :: T.Text -> Sum Int
countWordsInText txt = Sum $ length $ T.words txt


---

-- take the first 'n' words from a document for the summary
--
-- This simply counts the complete Str elements in the Inlines working from the
-- top block.   Note that it ignores headers or any other top level block links.
-- It only counts words in Plain or Para blocks.
-- The boolean is True if it did truncate, i.e. there were words left to take.

takeNWords :: Int -> TP.Pandoc -> (TP.Pandoc, Bool)
takeNWords n (TP.Pandoc meta bs)
  | n < 1 = (TP.Pandoc meta [], True)
  | otherwise = takeNWords' (n-1) meta 0 [] bs


-- The boolean is True if it did truncate, i.e. there were words left to take.
takeNWords' :: Int -> TP.Meta -> Int -> [TP.Block] -> [TP.Block] -> (TP.Pandoc, Bool)
-- we're done; just return the document.
takeNWords' n meta m ds [] = (TP.Pandoc meta (reverse ds), False)
takeNWords' n meta m ds (b:bs) =
    let (b', m') = case b of
            (TP.Plain ps) -> map' TP.Plain $ processMStr n [] m ps
            (TP.Para ps)  -> map' TP.Plain $ processMStr n [] m ps
            x             -> (b, m)
     in if m < n
          then takeNWords' n meta m' (b':ds) bs
          else (TP.Pandoc meta (reverse (b':ds)), True)
  where
      map' :: ([TP.Inline] -> TP.Block) -> ([TP.Inline], a) -> (TP.Block, a)
      map' cons (x, y) = (cons x, y)


processMStr :: Int            -- max number of Str elements
            -> [TP.Inline]    -- elements already processed in this set
            -> Int            -- number Str elements found so far
            -> [TP.Inline]    -- elements to process
            -> ([TP.Inline], Int)    -- resultant (elements, count)
-- processed everything, so return what has been processed so far.
processMStr _ as m [] = (reverse as, m)
-- it's a str so either as the string in, or just return what we have so far
processMStr n as m (i@(TPD.Str _):is)
  | m < n     = processMStr n (i:as) (m+1) is
  | otherwise = (reverse (i:as), m+1)
-- process the rest of the Inline types; most require recursing inline into the
-- structure
processMStr n as m (i:is) = case i of
    (TPD.Emph ls)        -> recurseIntoInline TPD.Emph is
    (TPD.Strong ls)      -> recurseIntoInline TPD.Strong ls
    (TPD.Strikeout ls)   -> recurseIntoInline TPD.Strikeout ls
    (TPD.Superscript ls) -> recurseIntoInline TPD.Superscript ls
    (TPD.Subscript ls)   -> recurseIntoInline TPD.Subscript ls
    (TPD.SmallCaps ls)   -> recurseIntoInline TPD.SmallCaps ls
    (TPD.Quoted qt ls)   -> recurseIntoInline (TPD.Quoted qt) ls
    (TPD.Cite cite ls)   -> recurseIntoInline (TPD.Cite cite) ls
    _                    -> processMStr n (i:as) m is
  where
      -- recurse into the Inline to find/count other Str and then carry on
      -- processing the remaining Str at the current level
      recurseIntoInline :: ([TP.Inline] -> TP.Inline) -> [TP.Inline] -> ([TP.Inline], Int)
      recurseIntoInline icons iis =
          let (iis', m') = processMStr n [] m iis
           in processMStr n (icons iis':as) m' is

---

-- Table of Contents
--
-- Generate a Table of Contexts from the Pandoc.  This will consist of a
-- set of plain headings and links.  The output document will be taken from a j2
-- fragment.  The TOC will be built from divs with classes and paragraphs/links.
--
-- First we have to collect the Headings into a TOC structure.


data TocItem = TocItem { tocTitle :: T.Text
                       , tocLink  :: T.Text
                       , tocLevel :: Int
                       }

instance Show TocItem where
    show TocItem {tocTitle=t, tocLink=l, tocLevel=n} = "T(" ++ show t ++ ", " ++ show n ++ ")"


extractToc :: TP.Pandoc -> [TocItem]
extractToc tp = DList.toList $ extractToc' tp

extractToc' :: TP.Pandoc -> DList TocItem
extractToc' = TPW.query queryHeaders

queryHeaders :: TP.Block -> DList TocItem
queryHeaders (TP.Header level attr lst) =
    DList.singleton $ TocItem {tocTitle=_title, tocLink=_link, tocLevel=level}
  where
      _title = T.concat $ map toStr lst
      (ident,_,_) = attr
      _link  = "#" <> ident
queryHeaders _ = DList.empty

-- now persist and parse TocItem to a Yaml item for storage
instance Y.FromJSON TocItem where
    parseJSON (Y.Object v) = TocItem
        <$> v .: "tocTitle"                                       -- site: <site-identifier>
        <*> v .: "tocLink"
        <*> v .: "tocLevel"
    parseJSON _ = error "Can't parse SitegenConfig from YAML/JSON"

instance Y.ToJSON TocItem where
    toJSON (TocItem tocTitle tocLink tocLevel)
      = Y.object [ "tocTitle" .= tocTitle
                 , "tocLink"  .= tocLink
                 , "tocLevel" .= tocLevel
                 ]


dumpToc :: [TocItem] -> BS.ByteString
dumpToc = Y.encode


-- would rather se decodeEither but it's deprecated; thus let's just use this
loadTocEither :: BS.ByteString -> Either String [TocItem]
loadTocEither bs = case Y.decodeEither' bs of
    Left e   -> Left (show e)
    Right ts -> Right ts


-- | extract the Table of Contents from a Pandoc document to a bytestring if it
-- needs to be saved.
extractTocItemsToByteString :: TP.Pandoc -> BS.ByteString
extractTocItemsToByteString = dumpToc . extractToc


-- | extract the TocItems out of the bytestring and into a Right
-- Left is a site error (suitable for raising).
byteStringToTocItemsEither :: BS.ByteString -> Either TE.SiteGenError [TocItem]
byteStringToTocItemsEither bs =
    case loadTocEither bs of
        Left e -> Left $ TE.OtherError $ T.pack $ "Could decode TocItems? : " ++ e
        Right ts -> Right ts


--
-- | render the TableOfContents from the TOC Pandoc that was previously
-- extracted.  This is a bit different as we don't render it until it's actually
-- asked for by the template (via Ginger).  Hence, this function expects the TOC
-- as a [TocItem] and then calls the relevant Pandoc utils to build the Pandoc
-- document from that.  This is then rendered to Text.
renderTocItemsToHtml :: Maybe T.Text -> Int -> [TocItem] -> Either TE.SiteGenError T.Text
renderTocItemsToHtml mStyle n ts =
    let pd = buildPandocFromTocItems n ts
        resultTxt = TP.runPure $ TP.writeHtml5String (pandocHtmlArgs mStyle) pd
     in case resultTxt of
        Left e    -> Left $ TE.PandocWriteError e
        Right txt -> Right txt


-- | build a Pandoc TOC div using TocItem to the level asked for.
-- This will be a <div id="toc"> and then and then a tree of <ul> inside divs
-- where each item is clickable and each div contains a ul of <a> links
buildPandocFromTocItems :: Int -> [TocItem] -> TP.Pandoc
buildPandocFromTocItems levels ts =
    let ts' = filter (\t -> tocLevel t <= levels) ts
     in B.doc $ B.divWith ("toc", [".toc"], []) (tocBuildList 1 ts')


-- build the list at level n
tocBuildList :: Int -> [TocItem] -> B.Blocks
tocBuildList _ [] = B.singleton TP.Null
tocBuildList n ts = B.bulletList $ tocStartList n ts


-- | initial build list -- we need to start at level 1, regardless of what the
-- first level is.  If it is a 1 then we just return the B.bulletList at level
-- 1, and find the rest at level 1, if it's at level n then try for that.
tocStartList :: Int -> [TocItem] -> [B.Blocks]
-- no items = no List Items
tocStartList n [] = []
-- test the first item
tocStartList n ts@(t:_) =
    let level = tocLevel t in case n `compare` level of
        EQ -> tocBuildListItems ts
        LT -> let (cs, rs) = L.break (\i -> tocLevel i == n) ts
               in tocMissingItem <> B.bulletList (tocStartList (n+1) cs) : tocStartList n rs
        GT -> error $ "The logic is broken: n(" ++ show n ++ ") > level(" ++ show level ++ ")"



tocBuildListItems :: [TocItem] -> [B.Blocks]
tocBuildListItems [] = []
-- if just the last item in the list, covert it to a link inside a Plain block
tocBuildListItems [t] = [B.plain (tocItemToLink t)]
-- compare the two items at the top of the list.  If same level then build a
-- list and recurse.  If the 2nd is higher than the first then embed the
-- subsequent lists into the same list item.
-- If the 2nd is lower than the first then we have a malformed TOC due to a
-- malformed set of headers.  However, we should already have dealt with this
-- before we get here, and so it's an error.
tocBuildListItems (t1:t2:ts) = case tocLevel t1 `compare` tocLevel t2 of
    -- they are the same, create an item and do the next one
    EQ -> B.plain (tocItemToLink t1) : tocBuildListItems (t2:ts)
    -- t1 < t2 means we have a sublist starting at t2.  find all the elements
    -- lower than t1 and group them and process them and then continue at the
    -- same level
    LT -> let levelT1 = tocLevel t1
              (cs, rs) = L.break (\t -> tocLevel t == levelT1) (t2:ts)
           in B.plain (tocItemToLink t1) <> tocBuildList (levelT1 + 1) cs : tocBuildListItems rs
    -- something wierd happened.  We can't go from a 2 - 1 (for example) as we
    -- are supposed to have already dealt with that issue
    GT -> error $ "tocBuildListItems can't deal with " ++ show (tocLevel t1) ++ " > " ++ show (tocLevel t2)


-- make a Pandoc Link from a TocItem
-- adds a class .tocLevelN to for the level
tocItemToLink :: TocItem -> B.Inlines
tocItemToLink TocItem {tocTitle=title, tocLink=link, tocLevel=level} =
    B.linkWith ("", [T.pack $ ".tocLevel" ++ show level], [])
               link
               title
               (B.text title)


-- add a Plain text item that stands in for a missing heading
tocMissingItem :: B.Blocks
tocMissingItem = B.plain $ B.text "<missing heading>"


-- adhoc testing of toc lists
testToc = [ TocItem {tocTitle="t0.1.1", tocLink="#0", tocLevel=3}
          , TocItem {tocTitle="t1", tocLink="#1", tocLevel=1}
          , TocItem {tocTitle="t2", tocLink="#2", tocLevel=1}
          , TocItem {tocTitle="t3", tocLink="#3", tocLevel=1}
          , TocItem {tocTitle="t3.1", tocLink="#4", tocLevel=2}
          , TocItem {tocTitle="t3.2", tocLink="#5", tocLevel=2}
          , TocItem {tocTitle="t3.2.1", tocLink="#6", tocLevel=3}
          , TocItem {tocTitle="t3.3", tocLink="#7", tocLevel=2}
          , TocItem {tocTitle="t3.3.1", tocLink="#8", tocLevel=3}
          , TocItem {tocTitle="t3.3.2", tocLink="#9", tocLevel=3}
          , TocItem {tocTitle="t3.3.3", tocLink="#10", tocLevel=3}
          , TocItem {tocTitle="t3.3.3.1", tocLink="#11", tocLevel=4}
          , TocItem {tocTitle="t3.4", tocLink="#12", tocLevel=2}
          , TocItem {tocTitle="t4", tocLink="#13", tocLevel=1}
          , TocItem {tocTitle="t4.x.1", tocLink="#14", tocLevel=3}
          , TocItem {tocTitle="t4.1", tocLink="#15", tocLevel=2}
          ]

-- this was the testing function
{-runTest x = TP.runPure (TP.writeHtml5String TP.def x)-}

runTest :: Int -> FilePath -> IO ()
runTest n fp = do
    let pd = buildPandocFromTocItems n testToc
        html = fromRight "" $ TP.runPure (TP.writeHtml5String TP.def pd)
    writeFile fp (T.unpack html)

runTest2 :: Int -> TP.Pandoc
runTest2 n = buildPandocFromTocItems n testToc
