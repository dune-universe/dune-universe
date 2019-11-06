v4.0.1 2019-11-04
-----------------

* provide deprecated Mirage_channel_lwt for smooth transition (#31 @hannesm)


v4.0.0 2019-10-24
-----------------

- remove mirage-channel-lwt, fold Make(Flow):S into mirage-channel (#29 @hannesm)
- mirage-channel specialised on Lwt.t and Cstruct.t (#29 @hannesm)
- raise lower OCaml bound to 4.06.0 (#29 @hannesm)

v3.2.0 2019-02-07
-----------------

- Port build to Dune (@avsm)
- Fix ocamldoc format to be odoc-clean (@avsm)
- Update opam metadata to 2.0 format (@avsm)
- Update test matrix to OCaml 4.07 (#24 @hannesm)
- Use io-page-unix instead of io-page.unix (#24 @hannesm)

v3.1.0 2017-06-14
-----------------

- Port build to Jbuilder.

v3.0.0
------

Adapt to MirageOS 3 CHANNEL interface:

- use `result` instead of exceptions
- hide `read_until` as an internal implementation
- remove `read_stream` from external interface as it is
  difficult to combine Lwt_stream and error handling.

v1.1.1 2016-10-20
-----------------

- port to topkg and odig conventions

v1.1.0 2016-06-28
-----------------

- don't call `close` on `Eof`
- add `read_exactly`
- add LICENSE
- add conflict with old versions of TCP/IP
