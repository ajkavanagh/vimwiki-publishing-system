# The internal server

VPS provides an internal webserver which can be used for development.  It should
not be used for production.  It is accessed using the -s option (and optional
-p) option.  (This may change to "sitegen server" and "sitegen build" as
sub-commands).

The server scans the source directories for source files and builds the initial
list, including indexes (and virtual index) SourceMetadata objects.

It then puts a Filesystem watch on the source directories (not the statics) to
watch them for changes.
