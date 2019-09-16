## v1.5.1 (2019-09-14)
* Report errors correctly from read/write (#29 @magnuss)
* Remove deprecated calls to `Cstruct.set_len` (#29 @magnuss)
* Update to use Macaddr and Macaddr.sexp 4.0.0 (#28 @magnuss)

## v1.5.0 (2019-02-14)
* Expose the MTU via `Lwt_vmnet.mtu` as well (@avsm)
* Use `Lwt_dllist` to remove deprecated `Lwt_sequence` (@avsm)

## v1.4.0 (2019-02-13)
* Expose the MTU of the interface via `Vmnet.mtu` (@avsm)

## v1.3.3 (2019-01-25)

* Switch to using macaddr library in ipaddr.3.0.0+ (#25 @hannesm)
* Minor ocamldoc fixes (@avsm)

## 1.3.2 (2018-12-26)

* compile with Lwt 4 (#22 by @anmonteiro)
* port to dune from jbuilder

## 1.3.1 (2018-01-23)

* remove `-warn-error` flags in release mode. Fix compilation on 4.06
  (#21, @samoht)

## 1.3.0 (2017-06-24)
* Depend on cstruct-unix, part of cstruct 3.0.0

## 1.2.0 (2017-05-22)
* Fix a possible loss of events
* Clarify the limits of this (specifically no bridging) in the README
* Build via jbuilder

## 1.1.0 (2016-03-20)
* Replace camlp4 with ppx
* add LICENSE file

## 1.0.2 (2015-12-23)
* Add a test case for Vmnet write.

## 1.0.1 (2014-12-02)
* Raise the `Vmnet.Error` exception when interface init fails (#1, reported by @nojb)

## 1.0.0 (2014-01-12)
* Initial public release.
