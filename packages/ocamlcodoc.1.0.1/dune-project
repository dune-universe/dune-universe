(lang dune 1.10)

(generate_opam_files true)

(license BSD)
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://gitlab.inria.fr/tmartine/ocamlcodoc"))
(homepage "https://gitlab.inria.fr/tmartine/ocamlcodoc")
(bug_reports "https://gitlab.inria.fr/tmartine/ocamlcodoc")
(documentation "https://gitlab.inria.fr/tmartine/ocamlcodoc")

(package
 (name ocamlcodoc)
 (synopsis "Extract test code from doc-comments")
 (description "\
ocamlcodoc extracts the preformatted source code in OCaml
documentation comments, i.e. the code delimited by {[ ... ]} in
comments delimited by (** ... *). A typical usage is to write examples
in documentation comments that can be extracted and tested.
")
 (depends
  (dune (>= 1.10.0))
  cmdliner
  redirect
  (stdcompat (>= 10))))

