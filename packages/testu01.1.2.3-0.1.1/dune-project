(lang dune 2.8)

(name testu01)
(version 1.2.3-0.1.1)
(source (github LesBoloss-es/ocaml-testu01))
(documentation https://lesboloss-es.github.io/ocaml-testu01/)

(authors
 "Niols “Niols” Jeannerod <niols@niols.fr>"
 "Martin Pépin <kerl@wkerl.me>")
(license GPL-3.0-or-later)

(maintainers
 "Niols “Niols” Jeannerod <niols@niols.fr>")

(generate_opam_files true)

(package
 (name testu01)
 (synopsis "OCaml bindings for TestU01 1.2.3")
 (description "This package provides OCaml bindings for TestU01 1.2.3 TestU01 is
  a software library, implemented in C, and offering a collection of utilities
  for the empirical statistical testing of uniform random number generators.
  The OCaml bindings allow for easy testing of random number generators written
  in OCaml and that claim to be uniform.")
 (depends
  (ocaml  (>= 4.06.0))
  (md2mld (or :build :with-doc))))

;; FIXME: md2mld should only be a :with-doc dependency. However, because of the
;; use of the stanza (documentation (package testu01) (mld_files index)), the
;; documentation needs to be generated as part of the @install target, and
;; therefore md2mld is necessary at build time.
