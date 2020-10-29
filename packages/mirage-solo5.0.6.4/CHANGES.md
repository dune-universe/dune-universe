## v0.6.4 (2020-10-27)

* Synchronise interfaces with those of the newly-released mirage-xen:
    * Provide a new OS.Memory statistics interface.
    * Mark existing OS.MM interfaces as deprecated.
    * Use a Makefile to build `libmirage-solo5_bindings.a`.
    * Time: minor syntactic chagnes, remove unused `min_timeout`.
    * (@hannesm / @mato, #72)
* Remove superfluous conditional compilation for no longer supported OCaml versions (@hannesm, #70).

## v0.6.3 (2020-09-29)

* Adapt to bheap 2.0.0 API changes (@pascutto, #62)
* Add Metrics for sleep queue statistics (@hannesm, #63)
* Remove Time.Monotonic submodule (@hannesm, #65, #68, #69)

## v0.6.2 (2020-03-03)

* Remove redundant allocation in solo5_net_stubs.c. Fixes rare memory leak (@Reperator, #56)
* Port build system to Dune. (@hannesm, #58)
* Support OCaml 4.10.0. (@kit-ty-kate, #57)

## v0.6.1 (2019-11-02)

* Use at_enter_iter/at_exit_iter/at_exit hooks provided by Mirage_runtime
  since 3.7.0 (#54 @dinosaure, see mirage/mirage#1010)
* Remove exception handling of Lwt.poll (#55 @hannesm, see mirage/mirage#1011)
* Require OCaml 4.06.0 (#55 @hannesm)

## v0.6.0 (2019-09-24)

* Update to Solo5 0.6.0+ APIs, multiple devices support (@mato, @hannesm, #46)
* Provide a malloc statistics source (@hannesm, #43)
* Replace deprecated modules (@pascutto, #42)
* Install mirage-solo5.pc to lib/pkgconfig instead of share/pkgconfig
  (@hannesm, #40)

## v0.5.0 (2018-11-08)

* Block, net stubs: Pass through Cstruct.buffer.off (@mato, #38)
* Upgrade to OPAM 2 (@hannesm, #36)
* Solo5 bindings for Genode (@ehmry, #35, @mato, #39)
* Remove Env module (@hannesm, #34)

## v0.4.0 (2018-09-15)

* Update for Solo5 0.4.0 renaming
* Travis: Add OCaml 4.07 to build matrix

## v0.3.0 (2018-06-16)

* Update to Solo5 v0.3.0 APIs
* ARM64 support
* Support building on OpenBSD

## v0.2.1 (2017-06-15)

* Use [@@noalloc] rather than "noalloc"
* Fix warnings, formatting and cleanup
* Require OCaml 4.03
* Test also with OCaml 4.04
* Rename caml_{alloc_pages,get_addr} to mirage_{alloc_pages,get_addr}
  to avoid using the OCaml compiler namespace. This corresponds with
  [mirage/io-page#48]

## v0.2.0 (2017-01-17)

* Port to topkg (@mato, @hannesm, @yomimono, #15)
* Minor fixes and cleanups (various)

## v0.1.1 (2016-07-21)

* Initial release for publishing to opam.ocaml.org.
