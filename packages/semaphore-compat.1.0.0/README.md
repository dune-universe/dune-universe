## Compatibility `Semaphore` module
[![Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fmirage%2Fsemaphore-compat%2Fmain&logo=ocaml)](https://ci.ocamllabs.io/github/mirage/semaphore-compat)

Projects that want to use the `Semaphore` module defined in OCaml 4.12.0 while
staying compatible with older versions of OCaml should use this library instead.
On OCaml versions 4.12 and above, this library defines an alias `Semaphore` to
the standard library's definition of the module. Otherwise, it provides an
equivalent definition of `Semaphore`.

#### Depending on this library

Opam libraries depending on this module are encouraged to use a conditional
dependency:

```
depends: [
  ("ocaml" {>= "4.12.0"} | "semaphore-compat")
]
```

This ensures that any dependencies of your library that _don't_ need pre-4.12
compatibility will never transitively depend on `semaphore-compat`.

<hr/>

#### Acknowledgements

This repository structure is mostly copied from
[`JaneStreet/result`](https://github.com/janestreet/result), which provides a
compatibility `result` type. Thanks to the OCaml maintainers for their
improvements to the standard library.
