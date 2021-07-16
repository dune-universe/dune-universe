
## v1.1.0 (2021-07-15 )

* Improvements on --enable-share
* Remove link to typerex.org
* Add `opam-bin clean unused` to remove unused shared files

## v1.0.0 ( 2020-10-28 )

* Variable OPAM_BIN_EXCLUDE and config option `exclude_dirs` to list files
  to skip while computing checksums

## v0.9.5 ( 2020-09-14 )

* Fix a bug preventing creation of binary packages when sharing is disabled
* Add command 'opam-bin info' with a summary of opam-bin actions

## v0.9.4 ( 2020-08-31 )

* Use `drom` to manage the project
  * configure sphinx-target field to target docs/ directly
  * configure .github/workflows to skip Windows for missing deps
* Add sharing of files between switches:
  * Add `opam bin config --enable-share` to activate sharing during install
  * Add `opam share FILES` and `opam share -r DIR` to share a specific set
     of files

## v0.9.3 ( 2020-08-05 )

* Initial version
