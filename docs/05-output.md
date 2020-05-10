# Generating Output

When generating output, VPS needs to write files into directories and make sure
everything is lined up.  The topics in this page are concerned with how pages
are processed, how links are checked and fixed up, and what checks are done to
ensure that there aren't page clashes.

## index.html files

One of the site-get options is `index-files`, which defaults to `True` and means
that each 'page' is actually a directory with a single `index.html` file that
contains the context.  This means that the webserver can be configured to serve
an index file for the directory which cleans up slugs.  The option `index-files`
can be set to `False` to generate `slug.html` rather than `slug/index.html`
files.

Links are written appropriately to ensure that the correct link is presented
according to the way the files are written.

## Cleaning output

If the user decides to remove a file, the program needs to know whether it can
delete files that shouldn't exist.  Basically, the program *owns* the extension
(typically `.html`) in the output directories, and if the `clean` switch is used
on the command line, then post *successful* generation of the site, then files
that weren't generated (or didn't need updating) will be removed if they have
the output file extension.  If a directory is empty after this process, then
they will be deleted as well.

## Handling slugs in pages

One complication for pages is understanding how the `route` is used to create
the full route for the page.

Routes, ultimately, need to be stable, and obvious for the page, although it
should be possible that they are completely re-written.

The following items are taken into account for the page route:

 * The file path of the source file.  e.g. `./some/dir/page name.md` would map
   to `some/dir/page-name` as a slug.
 * The `route` override in the page header.  If `route` is `some-slug`, then the
   filename is replaced with the new route, and thus becomes
   `some/dir/some-slug`.  In this case, if *any* other page generates the same
   route the generation will stop *without writing a single file*.  If the
   `route` is prefixed with a `/` then the entire route is replaced with the
   `route`.  e.g. if `route` is `/archive` then the page is addressed as the
   page at `/archive.html` or `/archive/index.html` depending on whethe
   `index-files` is set.

### Site Pages

NOTE: I don't think these will make the cut.  Index pages ae a better idea.

Site pages are a special category of page, and I'm not entirely sure whether
they are needed.  The theory goes like this:

Let's say there is an *archived* area of the website.  i.e. pages that are
deliberately moved to the archive.  This could be controlled in the pages by
using a tag `archived`.

In the templates area, there will be a `/archived/index.hmtl.j2`.  However, we
don't actually need any content for the that page, just a way of activating it
and ensuring that it get's written.  The same is true for the site index, etc.

So we need an `index` site-page a `tags` site page and a `categories` site page
to generate those *collection* pages.  There *can* be context from the page, but
all that is needed is the sitegen header, to indicate that it is a site page and
where to put it.

Of course, we still need the program to automatically generate the pages for
each tag and category, and there's no real 'language' to do that with.  So,
we'll just make it part of the system; and that is what the sitegen config
`generate-tags` and `generate-categories` is for.  When set to `True` they cause
the program to create create pages for each tag and category in the
`tags/tag-name` and `category/category-name` spaces. The index pages are created
using the site-page feature.

## Index Pages

An `index-page` (if `True`) is a collecting page for a set of pages.  e.g.
posts, archives, diary, etc.  Note that *tags* and *categories* are
automatically generated index pages which then target the appropriate templates
to generate their pages (and sub-pages).

The other thing an index page does is set the `collection` to a list of the
pages in the directory that the index-page is in.  Note, it's an error to have
two index pages in the same source directory.

Thus the idea is to enable generic templates to be written to account for
different parts of the site.

## (Now Implemented) Templates, and how they are found

I've kind of glossed over how templates are picked for each page.  So, the
system attempts to *find* the template by look for more specialised templates
and then going to more and more generic ones.

If no template is specified then `default` plus the extension (e.g. `html.j2`)
is used to discover the template.  Then, the system searches for
a `default.html.j2` template in the same subdirectory of the `templates`
directory as the page source sits at.  If there is no file there, then the
parent directory is searched until the root of the templates is reached.  If
still no template is discovered then an error occurs.

If the template is defined and it has no path (i.e. it's just a file name) then
the same searching is done, and again, if nothing is found then an error
occurs.

If the template has a path (starting with `/`) then no search is done and the
the file is appended to the template directory.  If it doesn't exist then an
error occurs.
