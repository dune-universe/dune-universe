(lang dune 2.7)
(name waylaunch)
(version 0.2.0)

(formatting disabled)
(generate_opam_files true)
(implicit_transitive_deps false)

(license GPL-3.0-or-later)
(maintainers "Kate <kit.ty.kate@disroot.org>")
(authors "Kate <kit.ty.kate@disroot.org>")
(source (github kit-ty-kate/waylaunch))

(package
  (name waylaunch)
  (synopsis "Waylaunch is a program launcher for Wayland")
  (tags (wayland program launcher sway))
  (depends
    (ocaml (>= 4.11))
    base-unix
    (dune-configurator (>= 2.7))
    (bos (>= 0.2.0))
    (fpath (>= 0.7.0))
    (rresult (>= 0.6.0))
    (conf-pkg-config :build)
    (conf-libwayland :build)
    (conf-wayland-protocols :build)
    (conf-xkbcommon :build)
    (conf-cairo :build)
    (conf-pango :build)))
