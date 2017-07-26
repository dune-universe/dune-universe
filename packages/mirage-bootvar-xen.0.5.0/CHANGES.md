## v0.5.0 2017-07-12

* Port to jbuilder and modernise test matrix.

## v0.4.0 2017-01-20

* Remove examples directory, as the module is now directly used by the mirage front-end tool.
* Add an optional `filter` parameter to `argv`, allowing users to only pass those arguments which cmdliner might expect to see.
* Convert to topkg.
* Change name to mirage-bootvar-xen from mirage-bootvar.
* Move argv parsing to external library `parse_argv` and depend on that library.
* Build against MirageOS version 3, and drop support for earlier MirageOS versions.

## v0.3.2 2016-06-09

* Use astring instead of re.str (#18 by @jonludlam)
* Improve the parser to handle quoted strings and escaped characters; skip arguments that don't conform to 'a=b' rather than dying (#18 by @jonludlam)

0.3.1 (2016-02-07)
* Fall back to reading `OS.Start_info` if cmdline is not found in Xenstore (#13, by @jonludlam)
* Include the string in error message for malformed variables (#11, by @talex5)
* Enable warnings and remove an unused `Re` import (#11, by @talex5)
* Add travis.yml to Github repo (#12)

0.3 (2015-09-15)
* Add `Bootvar.parameters` to export the boot paramters (#5, by @Drup)
* Add `Bootvar.argv`, which reformat `Bootvar.parameters` to emulates
  Sys.argv. (#7, by @Drup)

0.2  (2015-03-19)
* get no longer raises exception
* add get_exn which raises Parameter_not_found on error
* return 'Ok of t | 'Error of msg in create instead of raising exception on error
* remove debug output

0.1 (2015-03-19)
* Initial release
