# Note taking?

Can we do something along the lines of Neuron for note taking?

I'm thinking we want to have an easy way of linking notes, which is obviously
to use the `[[link]]` type format.  Then we want to indicate associated
documents.  That's a minimal yaml header to `tags: [some, tags]`

Finally, we just need to be able to supply data to the page renderer to be able
to:

 1. Determine other related pages (by the tags?)
 2. Determine forward and back links (from the links in the page)
 3. Determine 'parent' pages based on the tags and or links?  i.e. we have to
    grow the hierachy dynamically based on the tags, parents, etc.

TODO: need to flesh this out a bit more.
