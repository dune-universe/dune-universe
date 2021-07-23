(lang dune 2.7)
(name dose3)
(version 7.0.0)
(generate_opam_files true)
(license "LGPL-3.0-or-later with OCaml-LGPL-linking-exception")
(authors "Pietro Abate" "Ralf Treinen" "Jaap Boender" "Johannes Schauer Marin Rodrigues" "Roberto Di Cosmo" "Felipe Garay" "Stefano Zacchiroli" "Jakub Zwolakowski" "Olivier Rosello")
(maintainers "Pietro Abate" "Johannes Schauer Marin Rodrigues" "Ralf Treinen")
(source (uri git+https://gitlab.com/irill/dose3.git))
(bug_reports https://gitlab.com/irill/dose3/issues/)
(homepage http://www.mancoosi.org/software/)
(documentation "https://irill.gitlab.io/dose3")

(package
 (name dose3)
 (synopsis "Dose library (part of Mancoosi tools)")
 (description "\
 The dose suite provides libraries for handling package meta-data, and various
 tools for analyzing package relationships in a large package repository.
  - dose-builddebcheck checks, given a collection of source package stanzas
    and a collection of binary package stanzas of Debian packages, whether
    the build-dependencies of each source package can be satisfied by the
    binary packages.
  - dose-distcheck checks for every package of a distribution whether it
    is possible to satisfy its dependencies and conflicts within this
    distribution.
  - ceve, a general metadata parser supporting different input formats
    (Debian, rpm, and others) and different output formats.
  - dose-outdated, a Debian-specific tool for finding packages that are not
    installable with respect to a package repository, and that can only be
    made installable again by fixing the package itself.
  - dose-challenged, a Debian-specific tool for checking which packages
    will certainly become uninstallable when some existing package is upgraded
    to a newer version.
  - dose-deb-coinstall, a Debian-specific tool for checking whether a set of
    packages can be installed all together.")
 (depends
  (ocaml (>= 4.03))
  (extlib (>= 1.7.8))
  (base64 (>= 3.1.0))
  (cudf (>= 0.7))
  (ocamlgraph (>= 2.0.0))
  (re (>= 1.2.2))
  stdlib-shims
  (ounit :with-test)
  (conf-python-3 :with-test)
  (conf-python3-yaml :with-test)
  (conf-dpkg :with-test)
  (odoc :with-doc)
 )
)

(package
 (name dose3-extra)
 (synopsis "Some extra functions for dose3")
 (depends
  (ocaml (>= 4.03))
  (extlib (>= 1.7.8))
  (camlbz2 (>= 0.7.0))
  (camlzip (>= 1.08))
  parmap
 )
)
