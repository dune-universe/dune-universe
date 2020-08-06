# http/af-caged

http/af-caged exposes a familiar `request -> response` interface, built as a
thin wrapper around [http/af][httpaf].

[httpaf]: https://github.com/inhabitedtype/httpaf

## Installation

Install the library and its dependencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install httpaf_caged
```

## Helpers

In addition to the server interface, http/af-caged includes parsers for some
common request headers. It currently includes parsers for cookie and accept
headers.
