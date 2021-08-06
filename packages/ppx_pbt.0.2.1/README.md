ppx_pbt
=========
Syntax extension for writing property based tests in OCaml code using properties
and generators abstraction.
Property based tests are written using [QCheck OCaml library](https://github.com/c-cube/qcheck)

Documentation
--------------
Documentation can be found [here](https://vch9.github.io/ppx_pbt/dev/ppx_pbt/index.html).

Test your program with ppx_pbt
---------------------------------

`ppx_pbt` can be integrated to your dune project with the following example:

```ocaml
(library
  (name foo)
  (libraries ppx_pbt)
  (inline_tests)
  (preprocess (pps ppx_pbt)))
```

`dune build` will ignore the attributes attached to your OCaml files.  
`dune runtest` will inline and execute QCheck property based tests.
