# vimwiki-publishing-system

The premise: allow writing in vimwiki, and publish as a website.  Obviously,
a wiki is already something designed to be published, so the challenge is to
make the output of the vimwiki *not* look like a wiki.

## Features that would be useful for the output sites

* Tags - generate a page of tags, and a page for each tag.
* Drafts (i.e. not publishing a page that is a draft)
* Removing links that point at drafts.
* Front page generation using template?
* Working with both vimwiki syntax and Pandoc markdown syntax.
* A yaml config to indicate templates, etc.

### YAML config

The configuration/control for the site build will be in a yaml file.  This will
specify the source and destination directories, how different parts are mapped
and what features are enabled in the site build.  Also, the location of template
files, and how they are controlled.

### Tags

The tag feature generates
