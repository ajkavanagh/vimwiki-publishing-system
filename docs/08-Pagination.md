# How Pagination works

So, if we have index pages, with a list of summary blocks for the pages in that
index, then we need a way of paging them into blocks.  Without that, the page
could end up being really deep.

In order to do this, sitegen needs to essentially, add the extra pages for the
index and provide a set of variables to the page that can be used in the
templates to provide the pagination: which page we are on, how many there are,
and the links to those pages.

## Variables / functions in a page that activate pagination

* a function `paginate()` that takes a list of items, and an optional maximum
  number of items, and returns a `Pager` object + the items for that page.
  `paginate(List[Object], max=Int) -> Pager`
  Note that this means that the `paginate()` function is context aware; it knows
  which page it is on, and this is reflected in the other variables.
  It's an error to call this more than once in a page.  Doing so will cause an
  exception and the page won't be rendered.
* To get those routes, we need a function that can return the list of routes.
  `selectPages(*criteria) -> List[Page]`
  Not entirely sure how this will work, but it should return the whole list of
  pages; the `paginate()` function then deals with that list.
* `Pager` : an object that is initialised by the `paginate()` function, and
  contains variables for the paging context.  It's also a top level object that
  is provided so that imported templates / blocks can use the pagination
  variables.
  * `ThisPager` -- the current page in the pagination.
  * `NumPagers` -- the number of pages in the pagination
  * `Items` -- the list of pages for the pagination.

Note that `Page.Route` is the route of the page, and will be something like
`index-2/` or similar.

Still todo is to:

 - [ ] write pagination.html.j2 includable template to provide a pagination
       block.  e.g. see https://www.w3schools.com/css/css3_pagination.asp for
       details on how it could work.
