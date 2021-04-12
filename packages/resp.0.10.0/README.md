resp — REdis Serialization Protocol library for OCaml
-------------------------------------------------------------------------------
v0.9-23-g3ebb681

resp is an OCaml library for working with the [Redis Serialization Protocol](https://redis.io/topics/protocol).
It provides a pure-OCaml streaming interface for building clients and servers that communicate using RESP.

resp is distributed under the ISC license.

Homepage: https://github.com/zshipko/resp

## Installation

resp can be installed with `opam`:

    opam install resp

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
resp`.

[doc]: https://zshipko.github.io/resp

## Tests

In the distribution sample programs and tests are located in the
[`test`](test) directory. They can be built and run
with:

    dune runtest
