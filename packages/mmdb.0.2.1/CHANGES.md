0.2.1
=====

* Removed 'build' constraint from ppx dependencies.

0.2.0
=====

* Introduced a separate exception type thrown by the library when an internal
  error occurs.

* Simplified error type returned by data lookup functions:

  * Removed `Invalid_node_number` and `Invalid_lookup_path` error variants
    since it should not be possible to provoke these errors through the
    high-level interface provided by this library. An exception will be thrown
    instead if these errors are encountered.

  * Removed the `Lookup_path_does_not_match_data` error variant since it simply
    indicates that a piece of information is not present in the database. The
    lookup functions will now return `Ok None` instead of `Error _` if the
    corresponding error code is returned by `libmaxminddb`.

0.1.0
=====

* Initial release
