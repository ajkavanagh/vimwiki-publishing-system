# TODO

[x] - Learn Polysemy for the Reader (config) and (state) links to pages.

[x] - Add publish-drafts flag to the the site.yaml config
[x] - Add --drafts option to the command line
[x] - Iterate through files
[x] - Extract the header from a file (if it has one)
[x] - Ignore files without a header
[x] - Ignore files that are draft (and the config isn't draft)
[x] - Find links
[x] - Ensure that internal links and routes are always lower case.
[x] - check for duplicate routes
[ ] - Render the file and write it
[x] - switch site-page to index-page in heades
[x] - Rename existing RouteContext to HeaderContext to reflect that it's just
      used internally to build the SourcePageHeader
[x] - Move HeaderContext into Header
[ ] - Rename SourcePageHeader to SourcePageContext
[ ] - Flesh out the variables that will be available to a rendering page.
[ ] - If there isn't a page identifying as the index, then have a boiler plate
      one with no content; i.e. just run the template for the index.

## Try just rendering the index.html file

Hard code the paths, etc, so we can try the whole rendering pipeline:

  `src/index.md` -> `SourcePageContext` -> "/index.html" route -> `RenderContext` ->
  rendered `index.html`.

* [x] Write a minimal test1.md  -- this is the site index in the example-site
* [x] Write a minimal index.html.j2
* [ ] Add a wrapper (HMTL) header/footer/sidebar block structure
* [x] Write the `SourcePageContext` definition.
* [x] Build the `SourcePageContext` from the test1.md file
  * [ ] This should reference that it's an index file and needs the index
        template.
  * [ ] It also contains the *route*
* [ ] Write the minimal `RenderContext` definition.  The `RenderContext` is the
      thing that is given to Ginger to actually render the file; it needs to
      contain all the variables (GVal m) that are needed to render the page.
* [ ] Build a `RenderContext` for the *route* `index` (this will actually
      eventually render to a file called `index/index.html` so that URLs can be
      `index` rather than `index.html`.
  * [ ] This will reference the template from that is derived from the
	`SourcePageContext` as it contains the slug for the index page.  i.e.
	when building the `RenderContext` then `SourcePageContext` provides the
	template for it.
  * [ ] The `RenderContext` will have all of the data necesssary to now render
        the file.  Note that we are not yet bothered about tags, categories and
        other links to files; this will come later.
* [ ] Render the `index/index.html` file using Ginger.
  * [ ] Workout how to map variables into the template; need to get them into
        `GVal` things.
