(lang dune 2.0)
(name ocaml_pgsql_model)
(version 0.1)

(generate_opam_files true)

(source (github pat227/ocaml-pgsql-model.git))
(license GPLv3)
(authors "papatangonyc@gmail.com")
(maintainers "papatangonyc@gmail.com")

(package
  (name ocaml_pgsql_model)
  (synopsis "An Ocaml library and utility for creating modules out of thin air that describe database tables and types, with functions for running queries and commands; Aka database modelling")
  (description "In the style of ODB for C++ or JOOQ for Java, this project aims to get off the ground rudimentary support for the creation of code (Ocaml modules) that otherwise would have to be tediously hand written, and maintained to track changes, such as to field names and types, in a live database. The output modules can serve as inputs to other client projects. This project support postgresql. For mysql support see the ocaml_db_model project")
  (depends
    (ocaml (>= 4.08.1))
    (bignum (>= v0.14.0))
    (core (>= v0.14.0))
    (fieldslib (>= v0.14.0))
    (postgresql (>= 4.0.1))
    (ppx_deriving (>= 4.5))
    (ppx_deriving_yojson (>= 3.5.2))
    (ppx_fields_conv (>= v0.14.0))
    (pcre (>= 7.2.3))
    (uint (>= 2.0.1))
    (yojson (>= 1.7.0))
  )
)
