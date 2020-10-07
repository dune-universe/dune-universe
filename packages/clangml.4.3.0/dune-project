(lang dune 1.11)

(generate_opam_files true)

(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://gitlab.inria.fr/memcad/clangml"))
(homepage "https://memcad.gitlabpages.inria.fr/clangml/")
(bug_reports "https://gitlab.inria.fr/memcad/clangml/issues")
(documentation "https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/index.html")
(version "4.3.0")

(package
 (name clangml)
 (synopsis "OCaml bindings for Clang API")
 (description "\
clangml provides bindings to call the Clang API from OCaml.
")
 (depends
  conf-libclang
  conf-ncurses
  conf-zlib
  (dune (>= 1.11.0))
  (stdcompat (>= 13))
  (ocaml (and (>= 4.08.0) (< 4.12.0)))
  (ocamlfind (and :build (>= 1.8.0)))
  (ocamlcodoc (and :with-test (>= 1.0.1)))
  (pattern (and :with-test (>= 0.2.0)))
  (metapp (>= 0.3.0))
  (metaquot (>= 0.3.0))
  (refl (>= 0.3.0))
  (odoc (and :with-doc (>= 1.5.1)))))
