# TODO

## Still to be done

[ ] Document how to make it all work.
[ ] Expand out the example site so that it serves as an example.
[ ] Doing less work:
    [ ] Enable detecting that the source file hasn't changed so that we don't
        bother processing it.
    [ ] Enable detecting that the output is the same as the previous version
        (if an output file exists) so that we don't write it out and change the
        dates.
    [ ] Ensure that index files don't need to be reprocessed if none of the
        dependent files have changed.
[ ] Generate a sitemap.xml file for the site as needed.
[ ] Generate an RSS feed for the site to describe what's new/published, etc.
    - [ ] Probably use: https://github.com/bergmark/feed
    - [ ] Generate RSS and Atom feeds.
    - [ ] link them somehow to some config or something.
[ ] implement functions for:
    - [o] absURL  -- currently disabled, as not sure what to do with it.
    - [ ] urlize (which might be the same as urlencode)
    - [x] enumerate(list) to provide [{item=n, item=item}]  -- we can't produce
          pairs, so we'll produce a list of dictionaries that have the index
          and them item in them.
    - [x] markdownify -- convert a Markdown string into html for inclusion
[ ] - Re-do logging and printing functions so that we can control the output
      nicely.
    - [x] Write a logging effect that provide severity and optional module
          logs.
    - [x] Write a Print effect that provides colorised printing output options.
    - [ ] Add a '--debug=xxx' option to extract the level of debugging wanted.
          Default is None.
    - [x] Go through the app to change all the log @String to either the print
          or the logging effect.
[ ] Need to work out / have a flag to generate a site without '/' prefixes on
    the URLs so that they can be explored using the file brower.  At present
    they stick to the /.  Perhaps something to do with the absURL function and
    removing the abs bit if a flag is set and just doing everything relatively?

## Done

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
[x] If there isn't a page identifying as the index, then have a boiler plate
      one with no content; i.e. just run the template for the index.
[x] Templates
  [x] Allow templates to be absolute, and don't do a search.  e.g. if the
      template is `/some-index` then only look for that file in the templates
      directory, and error if it isn't found.
  [x] If the page is an `index-page` then the default is `index` not
      `default`.
  [x] add a `_defaults` folder that contains the 'defaults' for the
      templates.  This is present at the root level and is the default that is
      searched for.
  [x] need to deal with 404.html page we want to custom generate it, and it
      should have a template.
[x] 404 pages
  [x] recognise a page with a `/404` route as part of 'normal' page rendering.
  [x] generate and recognise the 404 page particularly, 1st check that the
      template is available and route it directly in the `RenderContext`.
[x] Index Pages
  [x] ensure that the default for template is `index` if the page is an index
      page.
  [x] For every route level, and index page is required.  We have to generate
      the `SourceMetadata` for the route level if there is no explicit
      index page.
[x] Process the `<!--more-->` tag to say where a content summary ends.  e.g.
    we walk the processed Pandoc text elements and find the this item.  Note
    it only looks for the first one.  All the others are stripped.
[x] Work out from the `SourceMetadata` where to put an output file.
[x] Write output files, after we've worked out what the output filename is.
[X] Copy the static files across
[x] Generate category `SourceMetadata` pages for categories that don't
    have actual physical pages (each will be an index page, and if the
    corresponding `categories/<cat>` page doesn't exist then generate the
    `SourceMetadata`.  i.e. it's possible to have some content for
    a tag by making a page and giving it a route in the `categories/<cat>`
    route space.
[x] Generation tag `SourceMetadata` pages in the same vein as categories.
[x] Cache a template once it has been resolved -> this might be a bit complex,
    but it's probably possible to cache the resolved template name to
    a `Template SourcePos` thing and then just return them when they next need
    to be resolved.
[x] - make the content, summary and toc functions return `unsafeRawHtml` so
      they don't have to be pushed through `raw` to extract the text as raw
      which is what is ALWAYS wanted.
[x] Ensure that the pages that get rendered end up in the `sitePagesRendered`
    variable in the `SiteGenState`.  Otherwise, we could end up with duplicate
    things.
[x] Rationalise out the SourcePageContext and VirtualPageContext.  It's a bit
    pointless having both.  Turn them both into the `SourceMetadata` as that's
    a better description and stops them being confused with Context which is
    a Ginger variable thing.
[x] enable cleaning of output directories based on what is written and
    copied.
[x] Need a hugo directory replication mode for routes.  Essentially, it appears
    to take the directory name and strip off a date part and construct the
    link for the page from that + the slug (which we get from the route).  This
    is different to the organisation scheme that.  The problem is that the route
    and the location are different things in hugo, but are represented as the
    same in VPS.  Thus we need a hack to ensure that the route allows us to find
    the templates, etc, but the final file that is written goes to the correct
    place AND when links are processed, they end up pointing to the right thing.
[x] Add reading time to the app; work on 270 words per minute and could the
    words (Str items) in a document.
[x] Add pygments rendering and associated functions to handle it.  The
    advantage over hightlight.js is that it doesn't require JS to do the
    highlighting; it's done purely in HTML/CSS.
[x] Add template overrides directories.  i.e. a list of directories that can
    override what's in the theme.  The idea is to have a shared theme for
    a site, but then the site can override specific templates as it sees fit
    without having to copy the theme.  Could get messy?
[x] Re-do the static dir options so that we can have multiple ones; separate
    out the theme's static dir into a `theme-dir` option that will replace the
    `templates-dir` option.
[x] Add debug flag to command line to allow controlling how much printing is
    done.
[x] Find some way to cache the word count of a document so that it doesn't get
    recalculated all the time (performance)

I really like the idea of using the top level values as Initial caps, and
functions as lowercased.
