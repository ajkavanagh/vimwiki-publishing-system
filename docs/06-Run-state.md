# Run State - how we track what we are doing

So in the program, we need to know what we are doing.  Which means that we need
to collect together the various *mutable* and *immutable* parts of the state of
the application and bring them together.

I suspect that a `Reader` for each ummutable part, and a `State` for each
mutable part will be the most flexible approach, and then use the Polysemy
1.3.0 feature of `Bundle` to tie them together.  This will mean that I'll need
to upgrade Haskell and Stack to be able to handle the 1.3.0 code.

Anyway, the immutable parts are:

* `SiteGenConfig` configuration
* List of files to process in the form of `SourcePageHeader` records.
* map of `FilePath` -> `SourcePageHeader`
* map of route (`Text`) -> `SourcePageHeader`

The mutable parts of the application:

* The current `SourcePageHeader` being processed
* The html (body) to insert into the Ginger template.
* Any other bits associated with the current page.
