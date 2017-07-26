v1.4.0 2017-05-26
-----------------

* Port to [Jbuilder](https://github.com/janestreet/jbuilder) for build.

v1.3.0 2017-02-16
-----------------

* Port to MirageOS 3 interfaces.
* Improve Travis CI distribution coverage for tests.

v1.2.1 2016-08-16
-----------------

* Remove use of `lwt.syntax`. (#20, by @yomimono)
* Remove unused `id` type. (#19, by @talex5)

v1.2.0 2015-07-22
-----------------

* Remove the use of unescaped `Sys.command` (#12, by @hannesm)
* Add tests for read, write, mkdir, size; make them pass (#10, by @yomimono)

v1.1.4 2015-03-08
-----------------

* Add explicit `connect` signature into interface (#8).
* Add an `opam` file for OPAM 1.2 pinning workflow.
* Add Travis CI unit test file.

v1.1.3 2014-10-16
-----------------

* Fix `FS_unix.create` and `FS_unix.write`

v1.1.2 2014-09-11
-----------------

* Fix quadratic behavior (#5)

v1.1.1 2014-07-21
-----------------

* Prohibit directory traversal outside of exposed base directory
* Parent of base directory is base directory (/../ -> /)

v1.1.0 2014-06-09
-----------------

* Add an `FS_unix` module which implements `V1_LWT.FS`

v1.0.0 2013-12-16
-----------------

* First public release.
