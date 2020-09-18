(lang dune 2.5)
(name lua_parser)
(version 1.0.0)
(using menhir 2.1)

(generate_opam_files true)

(source (github drjdn/ocaml_lua_parser))
(license MIT)
(authors "Jason D. Nielsen")
(maintainers "drjdnielsen@gmail.com")

(package
 (name lua_parser)
 (synopsis "A Lua 5.2 Parser")
 (description "This is a parser and pretty-printer for lua 5.2")
 (depends
  (menhir (>= 20200624))
  (sexp_pretty (>= v0.14.0))
  (sexplib (>= v0.14.0))
  (ppx_sexp_conv (>= v0.14.1))
  (ppx_deriving	(>= 4.5))))