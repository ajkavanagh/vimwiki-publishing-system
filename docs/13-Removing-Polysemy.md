# Removing Polysemy

It seemed like a good idea to try out Polysemy as an effects system with which
to build the program.  However, whilst it's nice to be able to write 'pure' code
and still have effects, in practice it adds a lot of complexity and isn't really
needed when the whole thing just runs in the IO Monad.

There is, however, a lot of code!

## Driver to remove Polysemy

* It's complex
* The error messages are frankly very, very opaque.
* Lot's and lots of language extensions required.
* Longer compile times.
* (Possibly) longer run times, although much of the 'work' that VPS does is in
  parsing markdown files and then processing the Jinja2 style templates.
* It was very tricky to thread through Ginger and Pandoc; this makes it harder
  to support.

## Issues removing Polysemy

* What to replace it with?
* Sheer amount of code
* How to do it without spending months re-writing it?

## What to replace Polysemy with?

Well that's the issue. Options:

1. RWS Monad stack (Reader, Writer, State)  -- scratch that; WriterT is broken
   (space leaks)
2. The Handle pattern - https://jaspervdj.be/posts/2018-03-08-handle-pattern.html
3. Capabilities - https://github.com/tweag/capability - discounted - Not on
   stackage?  (would have to pull in via hackage)
4. The ReaderT pattern - https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
5. fused-effects - https://github.com/fused-effects/fused-effects
6. RIO - https://github.com/commercialhaskell/rio#readme

Note that RIO is essentially the ReaderT pattern with a separate Prelude.

I'm probably going to end up with the `ReaderT` design pattern, keep things very
shallow, and just recognise that everything is being done in `IO` anyway.

## How to do it?

Essentially, I need to remore all of the `Effect` modules that have been
created:

 * Cache   - parametric polymorphic caching in HashMaps (keys are `Text`)
 * File    - Essentially, just `IO` things, plus some conduit directory scans.
 * Ginger  - only types for threading the `Sem r` monad through Ginger
 * Locale  - Just `IO` things (very small)
 * Logging - Uses Colog with some default logging methods.
 * Print   - Essentially `IO` putStrLn and print with a quiet option
 * Time    - Just `getCurrentTime` within `IO`.

Then there are the other 'default' Polysemy effects that need to be replaced:

 * Error   - multiple error handling; which all throw to `IO`
 * State   - for the `SiteGenState` record
 * Reader  - for the `SiteGenReader` record

The file handling stuff is probably the only genuinely interesting interface
that could be split between API (or eDSL) and implementation.  e.g. getting
objects.

Pandoc can run pure, but Ginger (the templating system), wants a Monad to be
able to read files and write text to.  There's no reason that can't be a ReaderT
based monad.

This is also an opportunity to re-think how the program works.  At the moment,
it is a static site generator, which parses a complete set of files, generates
all the missing virtual files, and then writes a complete set of processed files
to defined locations (files) along with copied statics, etc.

What I think I want it to do is:

 1. Grab the configuration (needs IO)
 2. Scan the source directories and build-up source-metadatas (needs IO)

That gets enough metadata to be able to either do a complete static site.  Or
alternatively, to serve it on demand, as part of a development site.  Or maybe
even something else.

In essence, we have ("metadata" + object) is a source file. And source files
+ configuration objects + templates + statics => enough info fo the whole site.

Metadata => what the file is.  It might be by extension (e.g. `.md`) or in the
metadata.  And text files can have the metadata as part of the file.

So back to this.  In order to progress from the configuration and metadata, it
needs to build up the full site.  So the mutable variable is the `SiteState`.
And it holds for the context of the site:

 * A map of routes to SMs
 * A map of filenames to SMs
 * A map of vimwikilinks to SMs (this is a bit application specific).

Other mutable state for static site generation is:

 * memoed files; files that have been written during static site generation
 * The render list; all the SMs that have to be written.
 * All the routes that are to be rendered (to check for duplicates)
 * Errored files

For the development server, just need the routes available (static) + all the
files that get rendered.

---

In order to accommodate the needs of both the static site generator and the
development server / actual server, the tracing of the source files (metadata)
to things we can render needs to be dynamic as it is built up. i.e. it can't be
a static that is then used; hence the mutable state.

---

So let's hold the whole static + mutable in a single record (Env) in a ReaderT
and hold the mutable in a TVar/MVar that we access via some accessors through
the IO monad.

We'll use Lens to make accessing the record structure a bit nicer.  The MVar we
access though IO.

I also need a new name for VPS:

- pandoc site builder (PSB)
- {something} publishing system
- Text File Site builder (TFSB)
- 'fragments'?

---

I want to split my editing/publishing system into:

1. Content and Metadata as human readable files (mostly)
2. Repository of processed content/metadata
3. Publisher that takes the repository and can do things with it.

So 'content' starts off as human readable/writable, then gets processed into
a web format?  The metadata
