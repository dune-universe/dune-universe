## v1.12.0 (2019-06-26)
* Cope with frontend moving directly to Closed state (@talex5, #89)
* Remove colons in log prefixes (@yomimono, #91)
* Use mirage-xen.4.0.0 `Os_xen` interface (@TheLortex, #90)

## v1.11.0 (2019-05-05)
* Fix MAC address for netback devices (@talex5, #87).
  This changes the `CONFIGURATION` signature to provide both
  `read_frontend_mac` and `read_backend_mac`, and changes the XenStore
  implementation to return `fe:ff:ff:ff:ff:ff` for backends.

## v1.10.2 (2019-04-01)
* Use the new grant API provided by mirage-xen (@talex5 and @yomimono, #85).

## v1.10.1 (2019-03-20)
* Zero buffers before calling fill functions (@yomimono and @hannesm, #83).

## v1.10.0 (2019-02-24)
* Adapt to mirage-net 2.0.0 changes (@hannesm)

## v1.9.0 (2019-02-04)
* Port build from jbuilder to dune (@avsm)
* Switch to dune-release from topkg (@avsm)
* Clean up opam build depends and ppx (@avsm)
* Use lwt-dllist to remove deprecated `Lwt_sequence` (@avsm)

## v1.8.1 (2019-01-07):
* require OCaml 4.03, remove "result" dependency (#77, from @hannesm)
* require ipaddr 3.0 (#79, from @hannesm)

## 1.8.0 (2017-12-14):
* Support client-initiated shutdown (#67, from @talex5)
* Improve shared ring handling between Netback and Netfront (#68, from @talex5)
* Fix a number of build errors and improve documentation (various, from @djs55 and @talex5)

## 1.7.1 (2016-06-16):
* Switch to jbuilder
* Add topkg
* Split into mirage-net-xen and netchannel
* Add dependency on io-page-xen

## 1.7.0 (2017-01-24):

* Build against MirageOS version 3, and drop support for previous versions.
* Log exceptions from the `listen` callback.
* Use Cstruct.hexdump_pp to dump frames on error.
* Use the Logs library for logging.
* Use a monotonic counter for the RX id.

## 1.6.1 (2016-05-02):

* Generate a unique ID for each page shared with Xen for receiving data

## 1.6.0 (2016-04-09):

* Don't assume the first interface is meant when id can't be decoded
* Remove camlp4 dependency, add OCaml 4.02.ppx dependency
* Add simple xen-build Makefile target which tests via Docker

## 1.5.0 (2016-01-08):

* Generate a unique ID for each Netfront request. Before, we used
  the grant ref, but multiple requests may use the same ref. This could
  lead to pages being returned to the free pool before they had been
  read by netback.
* Add netback support, allowing mirage-net-xen to provide virtual
  network interfaces to other domains. This API is currently
  experimental.
* Add support for fragmented frames, including frames larger than one
  page.
* Use `Cstruct.fillv` instead of our own copy (which was incorrect, but
  the buggy path didn't get called unless the frame was fragmented,
  which we didn't support anyway).
* Use the offset field (previously we assumed it was always zero).
* `listen` now starts and returns the polling thread. Before, it
  returned a dummy thread that never resolved. This means that errors in
  the `listen` thread should now be reported.
* Errors in the user function passed to `listen` are now handled by
  `Lwt.async` (however that is configured). Before, we always printed
  the exception and then continued.
* Don't discard frames sent before `listen` is called.
* Split `Shared_page_pool` out into its own module.
* Fixed some compiler warnings.
* Require shared-memory-ring >= 1.1.1 (#25).
* Require io-page >= 1.5.0 for `page_size` (#21).

## 1.4.1 (2015-03-14):
* Wait for the backend network device to enter the `Connected` state
  before transmitting packets.  This fixes a race condition in a
  fast-booting unikernel that caused the first packet to be lost (#20, #23).

## 1.4.0 (2015-03-07):
* Add explicit `connect` function to interface (#19).

## 1.3.0 (2015-01-24):
* When waiting for space in the transmit queue, we would sometimes fail
  to notice when space became available. (#15)
* Copy out-bound data into pre-shared pages for performance, security
  and simplicity. (#17)
* Use a centrally sourced Travis file and test OCaml 4.02+ as well.

## 1.2.0 (2014-12-17):
* Add profiling tracepoints and labels (#13).
  Introduces a dependency on `mirage-profile`.
* New `opam` file present in source repository for OPAM 1.2 workflow.

## 1.1.3 (2014-08-08):
* Revert the serialization in 1.1.2 as Xen/ARM (4.5 and backport to 4.4)
  has been fixed to support granting the same page multiple times.
  Backport is in https://github.com/mirage/xen-arm-builder.

## 1.1.2 (2014-07-23):
* Wait for packets to be processed by the backend before returning from
  a `writev` call. Without this, the caller has no way to know when
  it's safe to reuse the buffer (#11).

## 1.1.1 (2014-05-27):
* Do not send oversized frames to the backend Netfront (#9 from Edwin Torok).

## 1.1.0 (2014-02-01):
* Depend on the unified io-page library instead of io-page-xen.
* Depend on new `xen-event` and `xen-grant` packages.

## 0.9.0 (2013-12-10):
* Add Travis CI scripts.
* Adapt to V1.NETWORK from mirage-types-0.5.0.
* Initial release based on mirage-platform-0.9.8 Netif.
