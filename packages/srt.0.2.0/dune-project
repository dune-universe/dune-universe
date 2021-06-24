(lang dune 2.1)
(version 0.2.0)
(name srt)
(source (github savonet/ocaml-srt))
(license GPL-2.0)
(authors "The Savonet Team <savonet-users@lists.sourceforge.net>")
(maintainers "The Savonet Team <savonet-users@lists.sourceforge.net>")

(generate_opam_files true)

(package
 (name srt)
 (synopsis "Binding for the Secure, Reliable, Transport protocol library")
 (description "\| Secure Reliable Transport (SRT) is an open source transport technology
              "\| that optimizes streaming performance across unpredictable networks, such
              "\| as the Internet.
              "\| This package provides OCaml bindings to the C implementation library.
 )
 (depends
  (conf-srt :build)
  (conf-pkg-config :build)
  (dune (> 2.0))
  (dune-configurator :build)
  ctypes
  integers
  posix-socket)
)
