(lang dune 2.7)
(name plist-xml)
(version 0.3.0)
(documentation https://alan-j-hu.github.io/ocaml-plist-xml/)
(generate_opam_files true)

(license MIT)
(authors "Alan Hu <alanh@ccs.neu.edu>")
(maintainers "Alan Hu <alanh@ccs.neu.edu>")
(source (github alan-j-hu/ocaml-plist-xml))

(package
  (name plist-xml)
  (synopsis
    "Reading and writing of plist files in the XML format in pure OCaml")
  (description
    "Reading and writing of plist files in the XML format in pure OCaml.

Implementation of https://www.apple.com/DTDs/PropertyList-1.0.dtd.")
  (depends
    (base64 (and (>= 3.4) (< 4.0)))
    (ISO8601 (and (>= 0.2.6) (< 0.3)))
    (markup (and (>= 0.8) (< 2.0)))
    (ocaml (>= 4.08))))

(package
  (name plist-xml-lwt)
  (synopsis "Reading of plist files in the XML format with Lwt")
  (description "Reading of plist files in the XML format with Lwt.")
  (depends
    (plist-xml (= 0.3.0))
    (lwt (and (>= 5.0) (< 6.0)))
    (markup-lwt (and (>= 0.5) (< 0.6)))))
