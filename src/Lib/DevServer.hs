module Lib.DevServer where

{-
  Provide a development server for reviewing the site.

  There are two parts to the server:

  * A FileWatcher that watches a specific directory or file.
  * An HTTP server that responds to requests, renders the page, and returns the
    rendered page.

  All of the SourceMetadata pages are generated for actual pages.  However, the
  SourceMetadata for virtual pages is not created; these are generated on the
  fly and rendered as needed (except if they don't exist; in which case, a simple
  404 page is returned).

  Thus the FileWatcher thread(s) have one job; to maintain the metadata that is
  read/processed from the physical source files (typically markdown files).
  This includes:

    * Changing an existing source file
    * Added a new source file
    * Deleting/removing a source file

  A watch is also done on the main yaml configuration file; if this changes,
  then everything is flushed and we start again.

  A main thread is used for the web-server.  It processes one request at a time
  (actually it can do multiple, but it uses a continuation to do that).

  Additional threads are used to watch the yaml configuration file and the
  directory that the source files live in.

  The worker threads all talk to the main thread using a BChan FileNotify where
  FileNotify is a data type that (currently) distinguishes between the config
  file and the source files that get changed.

  There is no live reload at present, but the idea is to allow injection of
  live-reload script.  For now, it's necessary to reload manually.
-}


