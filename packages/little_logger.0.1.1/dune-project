(lang dune 2.8)

(generate_opam_files true)

(name little_logger)

(package
 (name little_logger)
 (version 0.1.1)
 (license "MIT")
 (maintainers "Ryan Moore")
 (authors "Ryan Moore")
 (homepage "https://github.com/mooreryan/little_logger")
 (source (github mooreryan/little_logger))
 (documentation "https://mooreryan.github.io/little_logger/")
 (bug_reports "https://github.com/mooreryan/little_logger/issues")
 (synopsis "A tiny, little logger <3")
 (depends
  (async :with-test)
  (bisect_ppx :with-test)
  (core (>= "v0.12"))
  (core_bench (and (>= "v0.12") :dev))
  (easy_logging :dev)
  (dolog :dev)
  (ocaml (>= "4.08.0"))
  ppx_compare
  (ppx_inline_test :with-test)
  (ppx_expect :with-test)
  (ppx_sexp_conv :with-test)
  (ppx_jane :with-test)
  (re2 :with-test)))
