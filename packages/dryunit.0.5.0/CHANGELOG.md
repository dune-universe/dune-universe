Changelog
=========

0.5.0
-----

  * The main restrictions (originaly desined for `ppx_dryunit`) are gone. Using *relative paths* with `--cache-dir` and running outside build directories is now allowed.
  * Command `dryunit` outputs correctly to stdout (whenever a target list is absent).
  * Init templates were fixed so a file called `"tests.ml"` will be detected out of the box.
  * Small stability fix for bytes manipulation
