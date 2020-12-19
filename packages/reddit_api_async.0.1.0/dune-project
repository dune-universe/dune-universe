(lang dune 2.5)

(name reddit_api)

(version 0.1.0)

(source (github leviroth/ocaml-reddit-api))

(license MIT)

(authors "Levi Roth")

(maintainers levimroth@gmail.com)

(package
  (name     reddit_api_kernel)
  (synopsis "OCaml types for Reddit's API")
  (depends
    (odoc :with-doc)
    (dune        (>= 2.5))
    (cohttp      (>= 2.5.4))
    (core_kernel (>= v0.14.0))
    (ezjsonm     (>= 1.0))))

(package
  (name reddit_api_async)
  (synopsis "Async connection and utility functions for Reddit's API")
  (depends
    (odoc :with-doc)
    (dune              (>= 2.5))
    (async             (>= v0.14.0))
    (async_ssl         (>= v0.14.0))
    (cohttp-async      (>= 2.5.4))
    (core              (>= v0.14.0))
    (ezjsonm           (>= 1.0))
    (reddit_api_kernel (=  :version))
    (sequencer_table   (>= v0.14.0))))
