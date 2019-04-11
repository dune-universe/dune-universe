# papi — Performance Application Programming Interface for OCaml

v0.1.1

Papi provides OCaml bindings to
[PAPI (Performance Application Programming Interface)][papi-home], a C library
for portable access to hardware performance counters.

The bindings closely follow PAPI's [own interface][papi-docs]. As a consequence,
the multitude of errors that PAPI can signal are propagated to OCaml. User is
advised to at least skim the PAPI documentation.

Papi depends on the PAPI C library, version 5.4 or above.

Papi is distributed under the ISC license.

Homepage: https://github.com/pqwy/ocaml-papi

[papi-home]: http://icl.cs.utk.edu/papi
[papi-docs]: http://icl.cs.utk.edu/projects/papi/wiki/Main_Page

## Documentation

Interface files or [online][doc].

[doc]: https://pqwy.github.io/ocaml-papi/doc/papi/


[![Build Status](https://travis-ci.org/pqwy/ocaml-papi.svg?branch=master)](https://travis-ci.org/pqwy/ocaml-papi)
