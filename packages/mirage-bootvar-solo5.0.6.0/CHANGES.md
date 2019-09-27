## v0.6.0 (2019-09-24)

* Bump version number and dependencies to stay in sync with other Mirage/Solo5
  components.
* Update Travis Matrix, bump to Ubuntu Xenial, only test with last three
  compiler versions.
* Port to dune (#13, @pascutto)

## v0.3.0 (2018-06-17)

* Adapt to Solo5 v0.3.0 APIs.

## v0.2.0 (2017-01-17)

* Port to topkg (@hannesm, #6)
* Use common parse-argv library (@yomimono, #4)

## v0.1.1 (2015-07-15)

* Initial release for Solo5, derived from mirage-bootvar-xen.
* Only implements the Bootvar.argv interface as nothing else is required by
  current Mirage.
