(lang dune 2.0)
(name interval-map)
(version 0.1.0)
(documentation "https://dgllghr.github.io/interval-map/")
(source (github dgllghr/interval-map))
(license MIT)
(authors "Dan Gallagher")
(maintainers "Dan Gallagher")

(generate_opam_files true)

(package
 (name interval-map)
 (synopsis "An immutable interval map data structure")
 (description "\
An immutable interval map data structure implemented as an interval tree. Based
on [jgblight/im_interval_tree](https://github.com/jgblight/im_interval_tree).

Interval maps are great for finding intervals and their associated values which
overlap a given interval. This interval map supports intervals with excluded,
included, and unbounded bound ends. Multiple values may be associated with the
same interval.
")
 (depends
  (ocaml (>= 4.08.0))
  (dune (>= 2.0))
  (alcotest :with-test)
  (odoc :with-doc)))
