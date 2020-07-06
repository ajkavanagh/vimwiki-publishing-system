# TODO

[x] Learn Polysemy for the Reader (config) and (state) links to pages.

[x] Add publish-drafts flag to the the site.yaml config
[x] Add --drafts option to the command line
[x] Iterate through files
[x] Extract the header from a file (if it has one)
[x] Ignore files without a header
[x] Ignore files that are draft (and the config isn't draft)
[x] Find links
[x] Ensure that internal links and routes are always lower case.
[x] check for duplicate routes
[x] Render the file
[x] ... and write it
[x] switch site-page to index-page in heades
[x] Rename existing RouteContext to HeaderContext to reflect that it's just
      used internally to build the SourcePageHeader
[x] Move HeaderContext into Header
[x] Rename SourcePageHeader to SourcePageContext
[ ] Flesh out the variables that will be available to a rendering page.
[x] If there isn't a page identifying as the index, then have a boiler plate
      one with no content; i.e. just run the template for the index.
[ ] Templates
  [x] Allow templates to be absolute, and don't do a search.  e.g. if the
      template is `/some-index` then only look for that file in the templates
      directory, and error if it isn't found.
  [x] If the page is an `index-page` then the default is `index` not
      `default`.
  [x] add a `_defaults` folder that contains the 'defaults' for the
      templates.  This is present at the root level and is the default that is
      searched for.
  [ ] need to deal with 404.html page we want to custom generate it, and it
      should have a template.
[ ] 404 pages
  [ ] recognise and filter out any page with a `/404` route as part of
      'normal' page rendering.
  [ ] generate and recognise the 404 page particularly, 1st check that the
      template is available and route it directly in the `RenderContext`.
[x] Index Pages
  [x] ensure that the default for template is `index` if the page is an index
      page.
  [x] For every route level, and index page is required.  We have to generate
      the `VirtualPageContext` for the route level if there is no explicit
      index page.
[x] Process the `<!--more-->` tag to say where a content summary ends.  e.g.
    we walk the processed Pandoc text elements and find the this item.  Note
    it only looks for the first one.  All the others are stripped.
[x] Work out from the `SourceContext` where to put an output file.
[x] Write output files, after we've worked out what the output filename is.
[ ] Complete Ginger context variables for Index and Content pages.
[X] Copy the static files across
[ ] enable cleaning of output directories based on what is written and
    copied.
[ ] Generate category `VirtualPageContext` pages for categories that don't
    have actual physical pages (each will be an index page, and if the
    corresponding `categories/<cat>` page doesn't exist then generate the
    `VirtualPageContext`.  i.e. it's possible to have some content for
    a tag by making a page and giving it a route in the `categories/<cat>`
    route space.
[ ] Generation tag `VirtualPageContext` pages in the same vein as
    categories.
[ ] Generate a 404 `VirtualPageContext` if a `404` route page doesn't
    exist.
[ ] Document how to make it all work.
[ ] Expand out the example site so that it serves as an example.
[ ] Enable detecting that the source file hasn't changed so that we don't
    bother processing it.
[ ] Enable detecting that the output is the same as the previous version
    (if an output file exists) so that we don't write it out and change the
    dates.
[ ] Ensure that index files don't need to be reprocessed if none of the
    dependent files have changed.
[ ] Generate a sitemap.xml file for the site as needed.
[ ] Cache a template once it has been resolved -> this might be a bit complex,
    but it's probably possible to cache the resolved template name to
    a `Template SourcePos` thing and then just return them when they next need
    to be resolved.
[ ] implement functions for:
    - [x] absURL
    - [ ] urlize (which might be the same as urlencode)
    - [x] enumerate(list) to provide [{item=n, item=item}]  -- we can't produce
	  pairs, so we'll produce a list of dictionaries that have the index
	  and them item in them.
    - [ ] markdownify -- convert a Markdown string into html for inclusion
[ ] Top level keys
    - [ ] Title
    - [ ] Site
    - [ ] Data


I really like the idea of using the top level values as Initial caps, and
functions as lowercased.
