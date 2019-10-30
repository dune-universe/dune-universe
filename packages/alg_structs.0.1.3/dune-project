(lang dune 1.11)

(generate_opam_files true)

(name alg_structs)
(version 0.1.3)

(authors "Shon Feder")
(license MIT)
(source (github shonfeder/alg_structs))
(maintainers "shon.feder@gmail.com")
(homepage "https://github.com/shonfeder/alg_structs")
(bug_reports "https://github.com/shonfeder/alg_structs/issues")
(documentation "https://shonfeder.github.io/alg_structs/")

(package
 (name alg_structs)
 (synopsis "Interfaces and module combinators for algebraic structures")
 (description "
An experimental library for programming with algebraic and category-theoretic
structures in OCaml.

Currently, implemented structures include applicative, foldable, functor, monoid,
and semigroup.
")
 (depends
  (dune (>= 1.11.3))

  (ocaml (>= 4.08.0))
  (ppx_deriving (>= 4.4))
))

(package
 (name alg_structs_qcheck)
 (synopsis "Provides qCheck generators for laws of alg_structs")
 (description "
Combinators for generating qCheck property based tests to check that
implementations of the algebraic structures provided by alg_structs adhere to
the stated laws.
")
 (depends
  (dune (>= 1.11.3))

  (ocaml (>= 4.08.0))
  (alg_structs (and (>= 0.1.3) (< 0.2.0)))
  (qcheck (>= 0.11))

  (alcotest (and :with-test (>= 0.8.5)))
  (qcheck-alcotest (and :with-test (>= 0.11)))
))
