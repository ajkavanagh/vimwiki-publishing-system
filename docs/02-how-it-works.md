# How it works

## Collecting pages

- Tags
- Categories
- Diary pages (blog)  (not done yet)
- Updates (all pages that have been published/updated)

### Routes

So to keep it simple, the templates will be:

- / -> index.html.j2
- tags -> tags.html.j2
- tags/[tag] -> tag.html.j2
- categories -> categories.html.j2
- categories/[category] -> category.html.j2
- diary/ -> diary.html.j2
- diary/[data] -> diary-day.html.j2


## Modelling tags, categories and diary pages

The keys are strings and dates.  We also need to extract the 'natural' sort
order for the keys.  The collections are accessed by keys which are strings:

 - tags
 - categories
 - diary

### Tags & Categories

These are essentially a string => [`SourceMetadata`] items.  It almost
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
