(lang dune 1.10)

(generate_opam_files true)

(license BSD)
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://gitlab.inria.fr/tmartine/pattern"))
(homepage "https://gitlab.inria.fr/tmartine/pattern")
(bug_reports "https://gitlab.inria.fr/tmartine/pattern")
(documentation "https://gitlab.inria.fr/tmartine/pattern")

(package
 (name pattern)
 (synopsis "Run-time patterns that explain match failures")
 (description "\
pattern is a PPX extension that generates functions from patterns
that explain match failures by returning the common context and
the list of differences between a pattern and a value.
")
 (depends
  (dune (>= 1.10.0))
  (ppxlib (>= 0.9.0))
  (ppx_deriving (>= 4.4))
  (stdcompat (>= 10))))
