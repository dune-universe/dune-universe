(lang dune 2.7)
(cram enable)

(generate_opam_files true)

(name clim)

(version "0.3.0")

(source (github Ninjapouet/clim))

(package
 (name clim)
 (synopsis "Command Line Interface Maker")
 (description "It simplifies the binary command line definition and composition")
 (license "Apache 2.0")
 (authors "Julien Blond <julien.blond@gmail.com>")
 (maintainers "Julien Blond <julien.blond@gmail.com>")
 (homepage "https://github.com/Ninjapouet/clim")
 (bug_reports "https://github.com/Ninjapouet/clim/issues")
 (depends
  (cmdliner (>= 1.0.4)))
)

(package
 (name clim-ppx)
 (synopsis "Derivates cmdliner terms from type definitions")
 (description "This PPX gives to your program a nice CLI using simple type annotations")
 (license "Apache 2.0")
 (authors "Julien Blond <julien.blond@gmail.com>")
 (maintainers "Julien Blond <julien.blond@gmail.com>")
 (homepage "https://github.com/Ninjapouet/clim")
 (bug_reports "https://github.com/Ninjapouet/clim/issues")
 (depends
  (clim (= 0.3.0))
  (ppxlib (>= 0.14.0))
  (fmt (>= 0.8.8))
  (cmdliner (>= 1.0.4)))
)
