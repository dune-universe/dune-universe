## v2.1.0 (2019-02-12)
* Port to dune from jbuilder (@avsm)
* Use `lwt-dllist` instead of deprecated `Lwt_sequence` (@avsm)
* Upgrade opam metadata to 2.0 format (@avsm)

## 2.0.0 (2017-05-27):
* Change ocamlfind and opam package structure
  - xen-evtchn: same as before
  - xen-evtchn-unix: the package formerly known as xen-evtchn.unix
* Add Mirage ISC LICENSE.md
* Add archlinux deps

## 1.0.7 (2016-11-21):
* Modernize the opam file
* Modernize the travis configuration and test more distros
* Remove dependency on lwt.syntax
* Remove unnecessary depopt on xenctrl

## 1.0.6 (2015-02-17):
* Delay printing a scary message to when a client requests the Xen
  event channels are actually used. This avoids printing it in all apps
  compiled with conduit, used by cohttp.
* Add unit test to check loopback connections work.
* Add opam file.

## 1.0.5 (2014-06-19):
* Remove dependency on Unix which crept into 1.0.4

## 1.0.4 (2014-06-18):
* this software is now officially part of the Mirage project
* correct one file which hadn't got the ISC license header
* standardise indenting (ocp-ident --syntax=lwt -i)
* print a helpful error message if you haven't got the xen-evtchn module
  loaded.

## 1.0.3 (2014-06-14):
* xen-evtchn.unix package depends on xen-evtchn. This fixes linking problems
  in downstream libraries.

## 1.0.2 (2014-06-13):
* suppress exceptions in top-level bindings thrown when not able to talk
  to xen. Now this exception will be thrown when the app actually tries to
  use the event channel API, where it's possible to catch and deal with it.
* correct LICENSE to ISC (code is part of the Mirage project)
  This matches the per-file headers.

## 1.0.1 (2013-01-30):
* only build the C stubs if the xen headers are present

## 1.0.0 (2013-01-30):
* import event channel code from mirage
* fix segfault in calling 'pending' (OCaml type changed but C
  stub did not)
* open at-most-one interface per process by caching the connection
