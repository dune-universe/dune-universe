(lang dune 2.8)

(name swhid)

(license ISC)

(authors "Léo Andrès <contact@ndrs.fr>, Dario Pinto <dario.pinto@ocamlpro.com>")

(maintainers "Léo Andrès <contact@ndrs.fr>, Dario Pinto <dario.pinto@ocamlpro.com>")

(source
 (github ocamlpro/swhid))

(generate_opam_files true)

(using menhir 2.1)

(package
 (name swhid)
 (synopsis "OCaml library to work with Software Heritage identifiers")
 (description
  "swhid is an OCaml library to work with persistent identifiers found in Software Heritage")
 (tags
  (swh software heritage archive shwid persistent identifier))
 (depends
  (ocaml
   (>= 4.08))
  (digestif
   (>= 1.0.0))
  (jsonm
   (>= 1.0.1))
  (ezcurl
   (>= 0.1))
  (menhir
   (>= 2.1))
  (bisect_ppx
   (and
    :with-test
    (>= 2.6)
    :dev))))
