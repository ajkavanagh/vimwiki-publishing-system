# Themes and static files

It turns out there is a reason why most static site generators have a theme as
a concept: you need templates to use to render the files, but you also need
a set of static files to supply to the browser to make them look pretty.

However, there is a second set of static files: media and non-source files for
the target site.  Pictures, PDFs, or other non-source-processed files that need
to end up on the output site.

So this means we have multiple static directories that need to be dealt with.
So we have two options:

1. Introduce the concept of a *theme* that bundles a set of templates and
   static files into a *defined* set of directories, and then have a set of
   separate static directories that are content.  ... or
2. Just have a `static-dirs` that you can include the theme's statics under.

Obviously, option 2 is easier to code, but less satisfying from a conceptual
perspective.  Recognising that a *theme* has statics, and providing first class
support for the idea, helps with the conceptual model of how themes work and
what *statics* are for.

# `site.yaml` directives for themes and static directories

The `site.yaml` will have the following two directives that are affect static
files.

* `theme-dir` - this will point to a directory that has a `static`
  sub-directory that, if present, the contents of which will be copied to the
  output site.
* `static-dirs` - this will be an array of directories that will be copied to
  the output directory.

Internally, the templates directory and other parts will still be represented
as such.  The only changes are in the site config consumption and a new
`sgcStaticDirs` member, removing the `sgcStaticDir` option.
