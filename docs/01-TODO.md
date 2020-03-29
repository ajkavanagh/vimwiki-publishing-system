# TODO

[ ] - Learn Polysemy for the Reader (config) and (state) links to pages.

[x] - Add publish-drafts flag to the the site.yaml config
[x] - Add --drafts option to the command line
[ ] - Iterate through files
[ ] - Extract the header from a file (if it has one)
[ ] - Ignore files without a header
[ ] - Ignore files that are draft (and the config isn't draft)
[ ] - Find links
[ ] - Render the file and write it
[ ] - If there isn't a page identifying as the index, then have a boiler plate
      one with no content; i.e. just run the template for the index.

## Try just rendering the index.html file

Hard code the paths, etc, so we can try the whole rendering pipeline:

  `src/index.md` -> `SourceContext` -> "/index.html" route -> `RouteContext` ->
  rendered `index.html`.

* [x] Write a minimal test1.md  -- this is the site index in the example-site
* [x] Write a minimal index.html.j2
* [ ] Add a wrapper (HMTL) header/footer/sidebar block structure
* [ ] Write the `SourceContext` definition.
* [ ] Build the `SourceContext` from the test1.md file
  * [ ] This should reference that it's an index file and needs the index
        template.
  * [ ] It also contains the *route* that will specify the `RouteContext` file.
* [ ] Write the minimal `RouteContext` definition.
* [ ] Build a `RouteContext` file for the *route* `index` (this will actually
      eventually render to a file called `index/index.html` so that URLs can be
      `index` rather than `index.html`.
  * [ ] This will reference the template from that is derived from the
        `SourceContext` as it contains the slug for the index page.  i.e. when
        building the `RouteContext` then `SourceContext` provides the template
        for it.
  * [ ] The `RouteContext` will have all of the data necesssary to now render
        the file.  Note that we are not yet bothered about tags, categories and
        other links to files; this will come later.
* [ ] Render the `index/index.html` file using Ginger.
  * [ ] Workout how to map variables into the template; need to get them into
        `GVal` things.
