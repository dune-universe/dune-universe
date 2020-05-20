# Loga

[![Loga](https://circleci.com/gh/yutopp/loga.svg?style=svg)](https://circleci.com/gh/yutopp/loga)

*WIP*

A logging library for OCaml.

# HOW TO INSTALL

Clone this repository, then execute `opam pin`.

```
opam pin add loga . --strict
```

(TODO: Register to opam...)

# How to use

Add a `loga` and `loga.ppx` to your dune file.

Example:

```
(libraries loga)
(preprocess (pps loga.ppx)
```

Then you can use logger like below!

``` ocaml
let () =
    [%loga.info "Hello %s %d" "world" 42];
```

# How to run

``` 
dune runtest
```
