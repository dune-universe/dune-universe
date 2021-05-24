# node_of_ocaml

This is a simple OCaml ppx rewriter to use node modules in OCaml.

## Install

```shell
opam install node_of_ocaml
```

## Usage

In your `dune` file:
```dune
(executable
  (name <exe_name>)
  (modes js)
  (preprocess (pps node_of_ocaml)))
```

If you wish to use a node module, just use this:

```ocaml
let uniq = [%require uniq]
```

To use the compiled file in your browser you cin use your favorite JavScript bundler.
