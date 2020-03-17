(lang dune 2.2.0)
(name lambda_streams)
(version 0.1.2)
(source (github Risto-Stevcev/lambda-streams))
(license BSD-3-Clause)
(authors "Risto Stevcev")
(maintainers "me@risto.codes")
(generate_opam_files true)
(implicit_transitive_deps false)

(package
 (name lambda_streams)
 (synopsis "Lambda-based streaming library")
 (description "A simple and principled streaming library based on lambdas")
 (documentation "https://risto-stevcev.github.io/lambda-streams")
 (depends
  (ocaml (>= 4.06.1))
  (alcotest (and (>= 1.0.1) :with-test))
  (fmt (and (>= 0.8.8) :with-test))
  (mdx (and (>= 1.6.0) :with-test))
  (odoc (and (>= 1.5.0) :with-doc))
  (dune (>= 2.2.0))))

(package
 (name lambda_streams_lwt)
 (synopsis "Lwt helpers for lambda_streams")
 (documentation "https://risto-stevcev.github.io/lambda-streams")
 (depends
  (ocaml (>= 4.06.1))
  (lambda_streams (= :version))
  (alcotest (and (= 1.0.1) :with-test))
  (alcotest-lwt (and (= 1.0.1) :with-test))
  (lwt (>= 5.2.0))
  (dune (>= 2.2.0))))

(package
 (name lambda_streams_async)
 (synopsis "Async helpers for lambda_streams")
 (documentation "https://risto-stevcev.github.io/lambda-streams")
 (depends
  (ocaml (>= 4.07))
  (lambda_streams (= :version))
  (alcotest (and (>= 1.0.1) :with-test))
  (alcotest-async (and (>= 1.0.1) :with-test))
  (async (>= v0.12.0))
  (async_kernel (>= v0.12.0))
  (async_unix (>= v0.12.0))
  (core (>= v0.12.0))
  (dune (>= 2.2.0))))
