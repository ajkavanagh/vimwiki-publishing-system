# How it works

This is an attempt to workout how it actually works.  i.e. How do we generate
a site?

Sites need:

 - an index.html -- the start point
 - pages addressed from that start page.

But we also want a 'tags' page, and a 'categories' page (or parts of pages).

And, if it's a part that goes into a template, then it needs to be rendered
into the page template for every page from that template.

We also need a templating language that works with haskell.

Do we also need a simple concept of routes?

What would they be for?  We can hard code the 'categores' and 'tags' pages.
But we do need some concept of what the site looks like.

I think the most important bit is that we keep it relatively simple.  Thus we
just need to know the entry points (index, pages) and then this links to the
content.  It's a 'wiki', technically, so all the pages should match up.  It
would be nice to have dated articles though, so 'indexes' of pages is really
what I want.

So to drive the 'collecting' pages, this will need templates, and *shakespeare*
templates seem to be the chosen one.  (from the Yesod framework).  Now we just
need to link the templates to the collecting pages to understand what's going
on.

## Collecting pages

- Tags
- Categories
- Diary pages (blog)
- Updates (all pages that have been published/updated)

The point is to drive these from templates.  Also, want to have a 'page'
template to wrap around the content.

### Routes

So, there is a problem with how to do routes.

To identify these pages we need routes -> pages.

ie.

- / -> index page
- tags/ -> tags page (list of tags, and pages that have those tags)
- categories/ -> categories page (list of categories, and pages that have those
  categories).
- diary/ -> diary list page (list of diary pages)
- page/slug-name -> a particular page identified by the page itself.

So to keep it simple, the templates will be:

- / -> index.html.j2
- tags -> tags.html.j2
- tags/[tag] -> tag.html.j2
- categories -> categories.html.j2
- categories/[category] -> category.html.j2
- diary/ -> diary.html.j2
- diary/[data] -> diary-day.html.j2

Also, the flags to geneate them just decide whether to do it or not; the actual
templates and names of templates is wired in.  If they aren't available,
a warning is generated and it stopes.

We'll hard code these to start with; maybe later will add a routes.yaml or
equivalent mapping file.

## How processing can work

Something along the lines of:

* see if any of the *.[ext] files has changed (this implies either holding it
  in memory, or storing it in a file)
  * date of the file
  * Size of the file
  * Hash of the file has changed.

Each .[extension] file will have a `SourcePageContext` that contains the
information necessary to render the .[extension] file into an HTML fragment.

Rendering needs to be able to handle:

 - No SourceContext objects
 - Multiple SourceContext objects
 - Tags
 - Categories
 - Diary entries  -- pages with a template?

## Modelling tags, categories and diary pages

The keys are strings and dates.  We also need to extract the 'natural' sort
order for the keys.  The collections are accessed by keys which are strings:

 - tags
 - categories
 - diary

### Tags & Categories

These are essentially a string => [`SourcePageContext`] items.  It almost
certainly needs to be a list of those strings, so it's probably a list of
tuples, or a mapping and then the tags can be extacted as keys.

### Diary pages

*Let's Not implement this just yet.*

Diary pages are in the `diary/` subdirectory and a file looks like:
`2020-02-30.md` as a page.  Obviously, this doesn't have a `--- sitegen` stanza
and so has to be handled separately.

But, what's 'important' is that we probably don't want a separate diary page
for every page in the output site.  Rather, we want a page with the diary
entries on it.  We'd probably want all the pages for a month on a page?
