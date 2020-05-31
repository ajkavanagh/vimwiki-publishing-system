# Ginger Context issues

Understanding how the Context of `GVal m` values is created is slightly
complicated.  A 'simple' way to do it would be to attempt to produce it all up
front, and marshall it into a single, huge, GVal.

However, that's not really going to work for me.  I reckon that they will need
to be generated on the fly, particularly (maybe) for the top level keys.

I also need a way to 'register' in the top level keys with functions that
resolve the next level of keys (or at least the value).

So consider `navigation` items.  This will be things like:

* pages
* tags  - the tags and then the posts that map to those tags
* categories - the categories and then the posts that match those categories
* posts - all the posts (and the header data associated with it)
* site - the site config items

So something like *tags* needs to be generated from all of the headers.  But the
*posts* could be generated as needed.  But we should generate the *tags* on
demand.

So there's a few parts to the context:

1. We need a place to centralise the data.
2. We need an effect that can gather them together and index them with top level
   keys and convert them to `GVal m` values.
3. We need a lookup function that will use the effect and resolve the keys.

We could create out own `data` item and implement an instance of `ToGVal`. This
would allow the lazy conversion of the key to a value and allow the value to
generate things on demand.  Might be quite hard to do, especially in the `Sem r`
monad!

## Centralising the Data

We'll have a `Context.hs` that provides a `HashMap Text (Sem r (GVal (Sem r)))`
with some convenience functions.  The idea is that various things can register
with the context.  To do that, we'll put it in a *Polysemy* `State`.

## The Effect to gather stuff together

This will effectively be a `State`, but presented with a few handy helpers, and
a lookup function.  The lookup will return the value that can be converted to
a `GVal m`.  We probably want to leave it as Haskell values as long as possible
so that we can still do things with it in other contexts (e.g. counting the
posts).

## Converting the context(s) into `GVal m` values

This is so that they can be consumed in Ginger.  The issue is that they probably
need to be generated on demand.  This almost certainly means a custom data class
and then an instance on ToGVal/GVal so that they can resolve to something at the
appropriate time.

# DSL for interacting with the Context

We need to generate the mapping of top level keys (or anything else for that
matter) to things that generate those things.  Essentially, we can adopt one of
two principals:

 1. The Context module imports all the modules that want a 'context' item, and
    add the function and key from it.
 2. The Context module provides a library and interested modules can add
    themselves in.

If we adopt the 2nd approach, then we need a 'build' phase and then a use phase,
and they are effectively two different effects.

We don't really need an effect for the building approach.  We can just have
a function that does something like:

```haskell
registerIntoContext :: Context -> Context
```

i.e. given a context, add in a key to that `Context` object.  Then, within the
scope of the module, it performs calls of:

```haskell
registerContextKey :: Monad m => Context -> Text -> m (GVal m) -> Context
```

i.e. take a context and a key, register in a function that returns GVal m inside
a monad.  However, that's fairly awful, as we might want to access things on the
context in the native format.  Obviously, that's fairly difficult in Haskell as
you need a to have them in a variant, and that needs eventually to go to a
`GVal m` for the rendering.

So, the key is that the context brings together all of the things that might
need to be rendered, but the individual parts (like slugs -> pages) are
available as their own Effects.

Also, individual pages will have to include the context(s) that are relevant to
that page.  e.g. the Index will have different context keys than a context page.

# Variables available for various pages

An index page is generated at a route level, even if no page identifies itself
as an index page at that route level.  There will be no content, no tags and no
categories (for the index) and it will use the `index` template that is
available.

## Variables available in an Index Page

The following variable are available to an index page:

* `config` - the SiteGenConfig variable
* `page` - the variables from the page context
* `pages` - an array of page contexts from the *same* route as the index page.
  What this means is that if the index page has a route of `/things` then any
  pages with a route of `things/...` will be in the `pages` array.
  * `summary` - the 'above-the-fold' summary of the content
* `breadcrumbs` - an array (forwards from the route) of the route.  i.e. if
  this page is `some/things` then `breadcrumbs` will be `['some', 'things']`.
* `template` - the template that is being used to render the page.
* `ert` - (aspirational) -- the estimated reading time, based on 275 words per
  minute of the content.
* `content` - the html generated from any content in the page.  For most index
  pages, this is probably empty, but might be used.

## Variables available for a regular page

Note: they are essentially the same.

* `config` - the SiteGenConfig
* `page` - the variables from the page context
* `pages` - and array of page contexts at the same route level as this page.
* `breadcrumbs` as above
* `template` the template being used.
* `content` the html generated from the content of the page.
* `ert` - (aspirational) -- the estimated reading time, based on 275 words per
  minute of the content.
