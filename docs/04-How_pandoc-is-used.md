# How Pandoc is used in SiteGen

Note: Pandoc is used in LibVps in a very simple manner.  It's just used to
convert a single Markdown file into HTML as part of the vimwiki-to-html command
set.

This mini-document is to describe how Pandoc is used within the publishing
system, within the context of Ginger templates and the rest of the system.

## Markdown -> HTML converter

So Pandoc is used to generate HTML.  That's pretty much it.  Obviously, it's
a little more complicated than that, but not much.  These are the key points:

 * The HTML conversion is without a Pandoc template; i.e. it's a document
   fragment.
 * The AST needs to be read to convert vimwiki links - specially `[[some page]]`
   into `[some page](some page)`.  This is a bit more complex, and the initial
   version (for Vimwiki2HTML) was just to process the raw text.  This won't work
   because an escaped link (like above) would be converted.
 * Ordinary links need to be converted because page names (and their routes)
   need to be converted to the slugs that the links refer to.  i.e. a page can
   say what it's slug is, which, if referenced, would need to be converted.
 * Ordinary page names also need to be converted (as links).
   i.e. `The page name.md` is auto-slugged as `the-page-name` and any references
   would need to reference that.
 * Finally, the option to not have '.html' extensions generally means that every
   static file is actualy `name/index.html` is is referred to as `name`.  This
   allows the webserver to produce nicer slugs, but does mean that all the pages
   actually have to be generated with interesting filenames.

Thus Pandoc conversion is performed with an AST filter to find and convert the
links, and then to re-write the links according the the slugs help in the
`SourcePageContext` objects.  The tricky bit is the AST filter.

Also, it will be run with `runPure` as we don't want to run it in `IO`.  This
means we need to do all the file and IO handling ourselves.  But that's okay,
particularly as we will need to feed the output as the `body` to the Ginger
template.

## Converting Links

This is walking the AST and finding `Inline str` elements, searching of
`[[thing]]` or `[[thing|link]]` and converting them to an `Inline link ...`
element.  Obviously, the text before needs to be preserved and also the text
afterwards.

So an `Inline -> [InLine]` function is needed, but probably doesn't exist.  So
what is available is `[Inline] -> [Inline]` and then just match the the
`Inline str` elements, and maybe split them.


