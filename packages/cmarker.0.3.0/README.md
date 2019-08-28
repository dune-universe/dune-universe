# cmarker

This is Pablo's fork of [ocaml-cmark][1], originally by Jonathan Chan. I [had
need of the `unsafe` flag][4] for my [static site generator][5], and hadn't
heard back, so I forked it. I may expand to include functions for "examining,
modifying, and iterating over node data" which the original README says
"shouldn't be hard to do, just tedious."

Since I'm not officially a new maintainer, I'm calling this one `cmarker` until
I hear back 😛

# Differences

* Builds with [Dune][2], formats with [ocamlformat][6], because it's what I know.
* Includes the Unsafe flag for parsing, since I need it for my blog.

Many thanks to jyc for the initial version.

---

# ocaml-cmark

ocaml-cmark is an OCaml interface for the [cmark][3] CommonMark parsing and
rendering library.

Before installing this, you must install cmark.

# What's Implemented

All the parsing and rendering functions should be implemented.

The interface is listed in `src/cmark.mli`.

   [1]: https://github.com/jyc/ocaml-cmark
   [2]: https://github.com/ocaml/dune
   [3]: https://github.com/commonmark/cmark
   [4]: https://github.com/jyc/ocaml-cmark/pull/1
   [5]: https://github.com/pablo-meier/fleaswallow
   [6]: https://github.com/ocaml-ppx/ocamlformat
