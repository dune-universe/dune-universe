## mirage-stack — MirageOS signatures for network stacks

mirage-stack provides a set of module types which libraries intended to be used as MirageOS network stacks should implement.

The set of protocols defined is:

[Mirage_stack.STACKV4](stackv4) and [Mirage_stack_lwt.STACKV4](stackv4-lwt)

mirage-stack is distributed under the ISC license.

[stackv4]: https://mirage.github.io/mirage-stack/Mirage_stack.html
[stackv4-lwt]: https://mirage.github.io/mirage-stack/Mirage_stack_lwt.html

## Installation

mirage-stack can be installed with `opam`:

    opam install mirage-stack

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation and API reference is generated from the source
interfaces. It can be consulted [online][doc] or via `odig doc
mirage-stack`.

[doc]: https://mirage.github.io/mirage-stack/
