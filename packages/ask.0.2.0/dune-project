(lang dune 2.7)

(generate_opam_files true)

(name ask)

(version 0.2.0)

(authors "Marc Biedermann")

(source
 (github uzh/ask))

(license MIT)

(maintainers "marc.biedermann@econ.uzh.ch")

(homepage "https://github.com/uzh/ask")

(bug_reports "https://github.com/uzh/ask/-/issues")

(documentation "https://github.com/uzh/ask")

(package
 (name ask)
 (synopsis "Create/Answer questionnaires")
 (description "Generating and answering questions should be easy")
 (depends
  dune
  (ocaml
   (>= 4.11))
  (sihl
   (= 0.4.0))
  (sihl-storage
   (= 0.4.0))
  (conformist
   (= 0.4.0))
  (lwt_ssl
   (>= 1.1.3))
  (ppx_fields_conv
   (>= 0.13.0))
  (alcotest-lwt
   (and
    (>= 1.3.0)
    :with-test))
  (caqti-driver-mariadb
   (and
    (>= 1.2.1)
    :with-test))
  (ocamlformat :dev)))

(package
 (name ask-integrator)
 (synopsis "Link questionnaires to an uuid of 'a type")
 (description "Generate a link between questionnaires and types")
 (depends
  dune
  (ocaml
   (>= 4.11))
  (ask
   (= :version))
  (sihl
   (= 0.4.0))
  (sihl-storage
   (= 0.4.0))
  (conformist
   (= 0.4.0))
  (lwt_ssl
   (>= 1.1.3))
  (ppx_fields_conv
   (>= 0.13.0))
  (alcotest-lwt
   (and
    (>= 1.3.0)
    :with-test))
  (caqti-driver-mariadb
   (and
    (>= 1.2.1)
    :with-test))
  (ocamlformat :dev)))
