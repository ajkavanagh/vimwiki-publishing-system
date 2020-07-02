# Functions

Functions available from any page.

## `paginate(List[object], size=Int) -> Pager`

Produce a set of pagination pages for the current page.  Note that *only* the
first call actually produces the initial pager; if it is called more than once
in a page with different objects, then the result is essentially **undefined**.
Thus, don't do it!

The optional `size` parameter is to control the number of items on the page.
By default, it is 10.  The result is a `Pager` object.


## `selectPages(Str, include_self=Boolean) -> List[Pages]`

This selects a set of pages using the passed parameter as a Route.  By default
it filters out the route (or Pager route) for the current page.  This means
that it can be used to get a selection of pages for the current route, but also
include the current route (if wanted).  Note that `Pages` *is*
`selectPages(Page.Route, include_self=False)`

## `not(Boolean) -> Boolean`

Convert the first argument to its inverse. If there isn't an argument, then
this function defaults to `True`.


## `getlocale() -> Str`

This function is called by Ginger when the builtine `dateformat()` is called.
From https://ginger.tobiasdammers.nl/guide/syntax/filters/:

`dateformat(date, format, tz=null, locale=null)`

...

`locale` is the name of a locale; in order to resolve it, the `getlocale`
function is called. `getlocale` does not come with Ginger, you have to define
it yourself and add it to the Ginger execution context. This is because we want
to keep Ginger agnostic of the chosen host monad, and any method of using the
OS’s locale system would involve IO somehow. If `getlocale` is not defined,
date uses a “default locale” (roughly equivalent to the “C locale”).
