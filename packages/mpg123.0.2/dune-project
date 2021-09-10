(lang dune 2.0)

(name mpg123)

(generate_opam_files true)

(license "LGPL-2.1-or-later with OCaml-LGPL-linking-exception")

(authors "Michael Bacarella <m@bacarella.com>")

(maintainers "Michael Bacarella <m@bacarella.com>")

(homepage "https://github.com/mbacarella/mpg123")

(source
 (github mbacarella/mpg123))

(package
 (name mpg123)
 (synopsis "MP3 decoding library")
 (description "OCaml bindings to libmpg123")
 (depends
  (ocaml
   (>= 4.03))
  (dune
   (>= 2.0))
  dune-configurator
  conf-pkg-config
  ctypes
  ctypes-foreign
  conf-libmpg123))
