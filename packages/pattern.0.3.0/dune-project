(lang dune 1.10)

(generate_opam_files true)

(license "BSD-2-Clause")
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry-martinez/pattern.git"))
(homepage "https://github.com/thierry-martinez/pattern")
(bug_reports "https://github.com/thierry-martinez/pattern")
(documentation "https://github.com/thierry-martinez/pattern")
(version "0.3.0")

(package
 (name pattern)
 (synopsis "Run-time patterns that explain match failures")
 (description "\
pattern is a PPX extension that generates functions from patterns
that explain match failures by returning the common context and
the list of differences between a pattern and a value.
")
 (depends
  (ocaml (and (>= 4.08.0) (< 4.12.0)))
  (dune (>= 1.10.0))
  (metapp (>= 0.3.0))
  (metaquot (>= 0.3.0))
  (refl (>= 0.3.0))
  (ocaml-migrate-parsetree (>= 1.5.0))
  (stdcompat (>= 10))))
