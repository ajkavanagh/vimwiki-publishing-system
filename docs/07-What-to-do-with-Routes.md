# Routes, and all their hazzards

Pages have a route; I'm beginning to think it's a bad idea, but if it's used
sparingly then it might just work.

But within the program, the routes have to have a consistency, even if that
means re-writing them slightly.

So:

 - `/` is the route index page for the whole site.
 - `thing/` is an index page for the thing directory
 - `page1` is a page in the main (highest level) index.
 - `thing/page1` is a page in the `thing/` index.

And that's it.  No such controvery after all.  If a page header says it's an
index, but the route has no '/' on the end, then we add the '/' to the route.
If a page is not an index page, then we make sure there is not '/' on the end.

## Filling in routes

So, if we have a bunch of routes, we need to ensure that we fill in the index
pages.  We do by ensuring that every sub-route has a index route (one that ends
in '/').

### Special pages (404, categories, tags, diaries ...)

There are other pages that the system 'does', (e.g. 404) page that also need
virtual routes added if they don't exist.

An example is the 404 page.  This will get rendered if there is a template
called 404.html in either the templates root or `_default` directories.
However, a page *saying* it is the `404` route doesn't actually have to exist.
In this case a virtual SourcePageContext will be created that will generate the
page.  If no template exists then no page will be created.

The same goes for categories and tags.  If not real page grabs the route for
a category (e.g. category/papers) which also needs to be an index page, then
a virtual context will be created for it.  This means that a web author *can*
add content to a category index page.  The same goes for tags.
