(lang dune 1.10)
(generate_opam_files true)
(license "LGPL-2.1+ with OCaml linking exception")
(authors
   "Dominik Brugger <dominikbrugger@fastmail.fm>"
   "Oliver Gu <gu.oliver@yahoo.com>"
   "Philippe Veber <philippe.veber@gmail.com>")
(maintainers "Philippe Veber <philippe.veber@gmail.com>")
(source (github pveber/libsvm-ocaml))
(bug_reports "https://github.com/pveber/libsvm-ocaml/issues")
(homepage "https://github.com/pveber/libsvm-ocaml")

(package
  (name libsvm)
  (synopsis "LIBSVM bindings for OCaml")
  (description "
LIBSVM bindings for OCaml

LIBSVM-OCaml is an OCaml library with bindings to the LIBSVM
library, which is a library for Support Vector Machines.
Support Vector Machines are used to create supervised
learning models for classification and regression problems
in machine learning.
")
  (depends
     base
     (dune (>= 1.7))
     lacaml
     (ocaml (>= 4.03.0))
     stdio)
  (tags (clib svm)))
