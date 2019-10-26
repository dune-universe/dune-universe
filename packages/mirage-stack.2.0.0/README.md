## mirage-stack — MirageOS signatures for network stacks

mirage-stack provides a module types which libraries intended to be used as MirageOS network stacks should implement.

The signature defined is:

[Mirage_stack.STACKV4](stackv4)

mirage-stack is distributed under the ISC license.

[stackv4]: http://docs.mirage.io/mirage-stack/Mirage_stack/module-type-V4/index.html

## Installation

mirage-stack can be installed with `opam`:

    opam install mirage-stack

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

[![Build Status](https://travis-ci.org/mirage/mirage-stack.svg?branch=v1.1.0)](https://travis-ci.org/mirage/mirage-stack)

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
mirage-stack`.

[doc]: http://docs.mirage.io/index.html#package-mirage-stack
