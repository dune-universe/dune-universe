Archaeology: Raw import from the Tezos project

v0.1: Initial release

v0.2:
  - complete rewrite: the library pivots towards caches

v0.3:
  - Rename `Loose` as `Weak`
  - Rename `CACHE` as `CACHE_MAP` (and associated name changes) to hint at key-value storage
  - introduce set-caches (`CACHE_SET`) which are simple value caches (not key-value caches)
  - introduce `clear` to empty caches entirely
  - fix singleton-collection bug
  - simplify code in main entry module

v0.4:
  - Handle reinsertion as promotion
  - bugfix: do not hard-fail when removing the single element of a collection
  - Wrappers for Lwt, Lwt-Option, and Lwt-Result
  - rename Strict to Strong (as opposed to Weak)
