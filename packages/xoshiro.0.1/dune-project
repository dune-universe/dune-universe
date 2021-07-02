(lang dune 2.8)

(name xoshiro)
(source (github LesBoloss-es/xoshiro))
(documentation https://lesboloss-es.github.io/xoshiro/)

(authors
 "Niols “Niols” Jeannerod <niols@niols.fr>"
 "Martin Pépin <kerl@wkerl.me>")
(license LGPL-3.0-or-later)

(maintainers
 "Niols “Niols” Jeannerod <niols@niols.fr>")

(generate_opam_files true)

(package
 (name make-random)
 (version 0.1)

 (synopsis "Helper to build a module similar to Stdlib.Random")
 (description "This package provides helpers to build a module similar to the
  Random module of the standard library, by providing only a minimum amount of
  things (in the simplest case, only a `val bits : unit -> int` function)."))

(package
 (name xoshiro)
 (version 0.1)

 (synopsis "Xoshiro PRNGs as drop-in replacements for Stdlib.Random")
 (description "This package provides the Xoshiro family of pseudo-random
  number generators for OCaml, with an interface mimmicing that of the standard
  library. The Xoshiro generator are not cryptographically safe, but they
  provide better randomness than the standard library.

  By default, this package comes as C bindings for efficiency. If bindings are
  not welcome (eg. to compile to JS), one may depend on xoshiro.pure instead,
  providing the exact same interface, implemented as pure OCaml.")

 (depends
  make-random
  base-bigarray
  (core_bench :with-test)
  (testu01    (and :with-test (>= 1.2.3-0.2)))))
