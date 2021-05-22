# ocaml-httph
Minimal OCaml bindings to the [httpserver.h](https://github.com/jeremycw/httpserver.h) http server toolkit

## Summary
This library provides a _very_ minimal set of OCaml bindings to the httpserver.h header file. 
The advantage of this library is the underlying library is non-blocking, compact and reasonaby fast given my testing so far.
However, there are no bells and whistles here, just a very simple http toolkit.

## Installation
This library has no dependencies other than the httpserver.h header file vendored directly in the [lib](/lib) directory.
To run the examples you'll need `yojson` installed which you can grab in the standard way:
```
$ opam install yojson
```

## Benchmarks
See [BENCH.md](BENCH.md) for a start

## Alpha Notice
This is not production software, more of an experiment really.
