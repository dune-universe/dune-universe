## v3.0.1 (2019-11-04)

* provide deprecated Mirage_fs_lwt for smooth transition (#27 @hannesm)

### v3.0.0 (2019-10-23)

* Retire mirage-fs-lwt package (#25 @hannesm)
* Specialies io = LWt.t and page_aligned_buffer = Cstruct.t in mirage-fs (#25 @hannesm)
* Mark as deprecated, will be removed for MirageOS 4.0 (#25 @hannesm)
* Raise lower bound to OCaml 4.06.0 (#25 @hannesm)

### v2.0.0 (2019-02-28)

* Adjust to mirage-kv-lwt 2.0.0 interface (@hannesm)

### v1.2.0 (2019-02-02)

* Port to dune (@dinosaure)
* Test more OCaml versions (@hannesm)
* Fix doc urls (@dinosaure)
* Upgrade opam metadata to 2.0 (@hannesm)

### v1.1.1 (2016-06-29)

* Remove `open Result` statements

### v1.1.0 (2016-05-26)

* Port build to [Jbuilder](https://github.com/janestreet/jbuilder).

### v1.0.0 (2016-12-27)

* Import the `V1.FS` and `V1_LWT.FS` for mirage-types, part of MirageOS 3.0.

### v0.7.0 (2013-07-21)

* Add a `-nolwt` output mode which simply uses strings and has
  no dependence on Lwt.  For the modern user who demands ultra-convenience.

### v0.6.0 (2013-07-09)

* Adapt output to mirage-platform-0.9.2 `Io_page` API.

### v0.5.0 (2013-03-28)

* Added a -o option (needed for mirari)

### v0.4.0 (2012-12-20)

* Initial public release
