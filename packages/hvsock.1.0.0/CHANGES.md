## v1.0.0 (2017-07-04)

- Port to Jbuilder
- Minimum OCaml version is now 4.03.0

## v0.14.0 (2017-04-24)

- Update to MirageOS 3 (#51, @samoht)

## v0.13.0 (2017-01-20):
- use scatter-gather I/O to minimise system calls
- spawn a pair of blocking read/write threads, using a fixed amount
  of buffer
- switch to Lwt_preemptive.detach: 2x faster than Lwt_stream + run_in_main
- attempt to minimise the number of calls of Lwt_preemptive.detach

## v0.12.0 (2017-01-11):
- use a blocking connect by default, rather than a 300ms delay
- drop the OCaml heap lock in the non-blocking connect
- run the connect function in a background thread
- add appveyor and status badges

## v0.11.1 (2016-11-23):
- make connection fail within 300ms rather than 30ms to work better
  on virtualised systems

## v0.11.0 (2016-11-14):
- make connection fail within 30ms rather than 1s
- increase test coverage with more distros

## v0.10.0 (2016-07-25):
- major bug fixes in the Flow_lwt_hvsock_shutdown module
- extra robustness in case of unexpected errors from the Win32 API

## v0.9.0 (2016-07-14):
- Fix the signatures of the hvsock C stubs to include `unit` arg.

## v0.8.1 (2016-07-11)
- Fix build on OCaml 4.03
- Add an hvsock.lwt-unix subpackage for users of Lwt_unix

## v0.8 (2016-06-30)
- Increase flow buffer size to 4KiB from 1KiB
- Use Cstruct.t internally rather than marshalling to/from Byte.t
- Functorise to make compatible with Uwt as well as Lwt_unix

## v0.7 (2016-06-13)
- Treat unexpected `read` or `write` errors as `Eof in `FLOW`
- Treat unexpected `shutdown_read` and `shutdown_write` errors as `Eof`
- Clarify/add/update copyright headers in source files

## v0.6 (2016-05-25)
- Add a simple protocol with shutdown read, write and close

## v0.5 (2016-05-21)
- Bump Linux AF_HYPERV to 43
- Use one RX and one TX thread per connection
- Acquire the runtime lock before calling `uerror`
- Implement Lwt_hvsock.listen (untested)

## v0.4 (2016-05-12)
- Add an implementation of V1_LWT.FLOW

## v0.3 (2016-05-12)
- Avoid running out of Lwt_preemptive thread pool threads

## v0.2 (2016-05-12)
- Work around connect() blocking forever

## v0.1 (2016-05-12)
- Initial release
