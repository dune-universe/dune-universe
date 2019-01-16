## 1.6.0 (2019-01-11):
* Port to dune from jbuilder (@avsm)

## 1.5.4 (2017-07-05):
* Use `ppx_cstruct` directly instead of the `cstruct.ppx` compat
  package, which makes it easier for jbuilder subdirectory embedding.

## 1.5.3 (2017-06-16):
* Add missing dependency on io-page-xen

## 1.5.2 (2017-06-11):

* Add missing dependency on xenstore.client

## 1.5.1 (2017-05-30):
* Switch to jbuilder and topkg
* Update to shared-memory-ring-lwt

## 1.5.0 (2017-01-24)
* Build with MirageOS 3, and don't build against earlier versions
* support and test compiler versions 4.03 and 4.04
* remove unused `id` type

## 1.4.0 (2016-03-13):
* Remove dependency on camlp4; use ppx instead
* opam: now pass lint check
* use logs rather than printf
* blkfront: serialise calls to `connect`
* blkfront: strip deprecated prefixes like aio: from params keys

## 1.3.1 (2015-03-07):
* Add an explicit `connect` to the interface signature (#35)

## 1.3.0 (2015-01-29):
* Update to `io-page.1.4.0` interface.
* Add an `opam` 1.2 file for more convenient development.
* Simplify travis configuration via centralised scripts.

## 1.2.0 (2014-10-03):
* blkback: add 'force_close' to more forcibly tear down the device
* blkback: make 'destroy' idempotent
* blkback: measure ring utilisation; segments per request; total
  requests and responses (ok and error)
* blkback: support indirect descriptors (i.e. large block sizes)
* blkfront: if the 'connect' string is at all ambiguous, fail rather
  than risk using the wrong disk
* blkfront: use indirect segments if available

## 1.1.0 (2014-01-30):
* blkback: functorise over Activations.
* Depend on `xen-grant` and `xen-event` libraries rather than `xenctrl`.
* Remove `OS` module, depend on `io-page` (no `-unix` or `-xen`).
* Only use the high-level Activations.after interface.
* blkfront: allow 'connect' to take '51712' (bus slot) or 'xvda' or '/dev/xvda'
* blkback: improve performance through request merging
* blkback: functorise over Activations, xenstore and backing store
* blkback: add the toolstack-level device create/destroy functions

## 1.0.0 (2013-12-10):
* Add ISC copyrights everywhere.
* Indent code with `ocp-indent`.

## 0.5.0 (2013-12-08):
* Block.connect: if we don't recognise the id, default to the
  first available disk.

## 0.4.0 (2013-12-08):
* implement new mirage-types BLOCK interface

## 0.2.5 (2013-11-10):
* fix build against cstruct.0.8.0
* blkfront write_page only needs to grant read access to
  the backend driver

## 0.2.4 (2013-10-13):
* fix reading non-page aligned sectors

## 0.2.3 (2013-10-05):
* add Travis continuous integration scripts.
* use new Activations.after interface

## 0.2.2 (2013-08-08):
* update to mirage-platform 0.9.4

## 0.2.0 (2013-07-17):
* blkback has significantly increased performance through
  mapping all requests on the ring simultaneously

## 0.1.3 (2013-07-14):
* 32-bit guest support fixed
* fixed critical bug in previous version which caused pages
  to be mapped with the wrong protection flags, leading to
  immediate segfaults on write

## 0.1.2 (2013-07-04):
* updated following mirage-xen Gnttab API change

