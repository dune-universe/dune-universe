
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
