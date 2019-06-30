# phonetic

Phonetic algorithm in OCaml

|            | Linux              | macOS              |
| ---:       | :---:              | :---:              |
| OCaml 4.05 | ![linux-ocaml4.05] | ![macos-ocaml4.05] |
| OCaml 4.06 | ![linux-ocaml4.06] | ![macos-ocaml4.05] |
| OCaml 4.07 | ![linux-ocaml4.07] | ![macos-ocaml4.05] |
| OCaml 4.08 | ![linux-ocaml4.08] | ![macos-ocaml4.05] |

[linux-ocaml4.05]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/1
[linux-ocaml4.06]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/2
[linux-ocaml4.07]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/3
[linux-ocaml4.08]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/4
[macos-ocaml4.05]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/5
[macos-ocaml4.06]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/6
[macos-ocaml4.07]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/7
[macos-ocaml4.08]:https://travis-matrix-badges.herokuapp.com/repos/geneweb/phonetic/branches/master/8

## Installation

```bash
opam install phonetic
```

## What's inside

- `DoubleMetaphone.double_metaphone`:
  [Double Metaphone](https://en.wikipedia.org/wiki/Metaphone#Double_Metaphone)

## Instructions for developpers

```bash
dune build            # build the library
dune install          # install the built library
dune runtest          # run unit tests
dune build @runbench  # run benchmark
dune clean            # clean compilation artifacts
```
