(lang dune 2.0)
(version 0.3.0)
(name flac)
(source (github savonet/ocaml-flac))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name flac)
 (synopsis "Bindings to libflac")
 (depends
  conf-libflac
  conf-pkg-config
  (dune (>= 2.0))
  dune-configurator)
 (depopts
  (ogg (>= 0.7.0)))
)
