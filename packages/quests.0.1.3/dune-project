(lang dune 2.0)
(generate_opam_files true)
(name quests)
(version 0.1.3)
(documentation "https://roddyyaga.github.io/quests/quests/index.html")
(homepage "https://github.com/roddyyaga/quests")
(bug_reports "https://github.com/roddyyaga/quests/issues")
(source (github roddyyaga/quests))
(license MIT)
(authors "Roddy MacSween <github@roddymacsween.co.uk>")
(maintainers "Roddy MacSween <github@roddymacsween.co.uk>")

(package
(name quests)
(synopsis "HTTP/1.1 client library like Python requests")
(depends
(ocaml (>= 4.06))
(dune (>= 2.0))
ssl
lwt_ssl
tls
lwt
cohttp
cohttp-lwt-unix
yojson
ppx_yojson
ppx_deriving
lwt_ppx
ezgzip
(alcotest :with-test)
(alcotest-lwt :with-test)))
