archeology: Raw import from Tezos

v0.1: Initial release

v0.2:
  - complete rewrite: the library pivots towards caches

v0.3:
  - Rename `Loose` as `Weak`
  - Rename `CACHE` as `CACHE_MAP` (and associated name changes) to hint at key-value storage
  - introduce set-caches (`CACHE_SET`) which are simple value cahces (not key-value caches)
  - introduce `clear` to empty caches entirely
  - fix singleton-collection bug
  - simplify code in main entry module
