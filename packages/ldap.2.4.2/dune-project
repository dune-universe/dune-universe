(lang dune 2.7)
(name ldap)
(version 2.4.2)

(formatting disabled)
(generate_opam_files true)
(implicit_transitive_deps true)

(license "LGPL-2.1-only with OCaml-LGPL-linking-exception")
(maintainers "Kate <kit.ty.kate@disroot.org>")
(authors "Eric Stokes <letaris@me.com>")
(source (github kit-ty-kate/ocamldap))
(documentation "https://kit-ty-kate.github.io/ocamldap")

(package
  (name ldap)
  (synopsis "Implementation of the Light Weight Directory Access Protocol")
  (depends
    (ocaml (>= 4.03.0))
    (ocamlnet (>= 3.6.0))
    pcre
    (ssl (>= 0.5.3)))
  (conflicts
    (ocamldap (<> transition)))
  (tags ("ldap")))
