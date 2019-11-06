## v2.0.1 (2019-11-04)

* provide deprecated Mirage_time_lwt for smooth transition (#12 @hannesm)

## v2.0.0 (2019-10-21)

* remove mirage-time-lwt (#10 @hannesm)
* specialise mirage-time to io being Lwt.t directly (#10 @hannesm)
* raise OCaml lower bound to 4.06.0 (#10 @hannesm)

## v1.3.0 (2019-06-18)

* Upgrade opam metadata to 2.0 format (#7 @hannesm)
* Upgrade to dune from jbuilder (#7 @hannesm)
* Improve test matrix to OCaml 4.07 and minimum is 4.04+ (@hannesm)

## v1.2.0 (2017-07-21)

* Add a mirage-time-unix module defining a `Time` module (#4, @samoht)

## v1.1.0 (2017-06-05)

* Port to [Jbuilder](https://github.com/janestreet/jbuilder) packaging.
* Add Travis CI tests.

## v1.0.0 (2016-12-20)

* Initial version, code imported from https://github.com/mirage/mirage
