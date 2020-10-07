# clangml: OCaml bindings for Clang.

clangml provides bindings for all versions of Clang, from 3.4 to
11.0.0.

## Introduction

This library is a complete rewritting of the previous clangml
(clangml versions <4.0.0):
the bindings now rely on automatically generated C stubs to libclang, with
some extensions when libclang is incomplete.
Contrary to old clangml versions, the versions of clangml from 4.0.0 are
independent from the version of the Clang library:
any version of clangml from 4.0.0 can be built with any version of the
Clang library in the supported interval.
Currently, all versions of Clang, from 3.4 to 10.0.1, are supported.

However, clangml is statically linked to libclang, and clangml needs
to be rebuilt for every version of libclang to run with.
In addition, the low-level bindings are automatically generated
from libclang's header and their signature can change from one version
of libclang to another.

The high-level bindings ([`Clang.Ast`], [`Clang.Type`],
[`Clang.Expr`], [`Clang.Stmt`], [`Clang.Decl`],
and [`Clang.Enum_constant`]) provide abstractions
that are essentially independent from libclang version.
These abstractions aim mainly to provide an algebraic datatype
representation of Clang abstract syntax tree (AST).
It is worth noticing that there can be some differences in the way clang
parses file from one version to another (in particular, some features of the
C/C++ languages are only supported by recent versions of clang,
see some examples in [`Clang__ast`] module documentation).

## Installation

clangml is installable via `opam`. Since the library relies on external
dependencies, we suggest to use the depext plugin to install it together
with the packages needed for your system:

```
opam depext -i clangml
```

Manual installation requires a bootstrapped source directory.
Commits from branch `snapshot` are bootstrapped: a new snapshot
is committed by continuous integration after every successful build from
`master`.

Snapshot tarball:
https://gitlab.inria.fr/memcad/clangml/-/archive/snapshot/clangml-snapshot.tar.gz

To build clangml from snapshot or from a bootstrapped source directory,
you may either:

* execute `./configure && make && make install`
(this method is recommended if you have to pass some options to configure);
* execute
`opam pin add git+https://gitlab.inria.fr/memcad/clangml.git#snapshot`.

To bootstrap the repository from a development branch (e.g., `master`),
execute `./bootstrap.sh` first, then `./configure && make && make install` as
usual.

clangml's `configure` relies on `llvm-config` to find clang's library.
By default, `llvm-config` is searched in `PATH`, and you may
specify a path with `./configure --with-llvm-config=...`.

clangml requires some dependencies:
`opam install dune refl`.
Additionnally, to run `make tests`: `opam install ocamlcodoc pattern`.

`libclang` and other external dependencies can be installed with opam depext
plugin:
```
opam pin add -n git+https://gitlab.inria.fr/memcad/clangml.git#snapshot
opam depext -i clangml
```
(`-n` option asks `opam pin` not to install clangml directly, and `-i` option
asks `opam depext` to install clangml once dependencies are installed.)

## Usage

The module [`Clang`][1] provides direct bindings to most of the [symbols
defined by libclang][2]. To match OCaml conventions, camel-case symbols
have been renamed to snake case (lower-case symbols with underscores), and
`clang_` prefixes have been removed. Additional bindings have been defined in
[`libclang_extensions.h`][3] for some parts of clang's API that have
not been covered by libclang.

[1]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/index.html
[2]: https://clang.llvm.org/doxygen/group__CINDEX.html
[3]: https://gitlab.inria.fr/memcad/clangml/blob/master/clangml/libclang_extensions.h

The module [`Clang.Ast`] provides a higher-level interface to clang's AST.
The function [`Clang.Ast.parse_file`] returns the AST from a file
and [`Clang.Ast.parse_string`] returns the AST from a string.
You may try these functions in OCaml toplevel to discover the resulting data
structure.

[`Clang.Ast.parse_file`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html#val-parse_file
[`Clang.Ast.parse_string`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html#val-parse_string

The module [`Clang.Ast`] includes in particular the module [`Clang__ast`]
which declares the algebraic data types that represent the AST.
The documentation of most of the nodes contains examples that can be used as references
for how syntactic constructions are parsed, and that are extracted with [`ocamlcodoc`]
and serve as unit tests with `dune runtest` (or, equivalently, `make tests`).
Moreover, the git branch [`norms`] contains the AST corresponding to the examples
automatically extracted from C++14, C++17, and C++20 norms.

[`Clang__ast`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang__/Clang__ast/index.html
[`ocamlcodoc`]: https://gitlab.inria.fr/memcad/ocamlcodoc
[`norms`]: https://gitlab.inria.fr/memcad/clangml/tree/norms/norms

Modules [`Clang.Type`], [`Clang.Expr`], [`Clang.Stmt`],
[`Clang.Decl`], and [`Clang.Enum_constant`] provides sub-modules
`Set`, `Map`, and `Hashtbl` as well as high-level abstractions to some libclang's bindings.

[`Clang.Ast`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html
[`Clang.Type`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html
[`Clang.Expr`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Expr/index.html
[`Clang.Stmt`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Stmt/index.html
[`Clang.Decl`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Decl/index.html
[`Clang.Enum_constant`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Enum_constant/index.html

In particular:

- given an expression node `e : Clang.Expr.t`, the type of `e` can be obtained by [`Clang.Type.of_node`] `e`;
- given a type `t : Clang.Type.t`, the alignment and the size of `t` can be obtained by [`Clang.Type.get_align_of`] `t` and [`Clang.Type.get_size_of`] `t` respectively;
- if `t : Clang.Type.t` is a `typedef`, the underlying type declared for `t` can be obtained by [`Clang.Type.get_typedef_underlying_type`]
- if `t : Clang.Type.t` is a record (`struct` or `union`), the list of fields can by [`Clang.Type.list_of_fields`];

[`Clang.Type.of_node`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-of_node
[`Clang.Type.get_align_of`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_align_of
[`Clang.Type.get_size_of`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_size_of
[`Clang.Type.get_typedef_underlying_type`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_typedef_underlying_type
[`Clang.Type.list_of_fields`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-list_of_fields

C/C++ attributes are defined in a separate (auto-generated) module
[`Attributes`].

[`Attributes`]: https://memcad.gitlabpages.inria.fr/clangml/doc/clangml/Clang__/Attributes/

The modules `Clang.Lazy.Ast`, `Clang.Lazy.Type`, `Clang.Lazy.Expr`,
`Clang.Lazy.Stmt`, `Clang.Lazy.Decl`, and `Clang.Lazy.Enum_constant`
mirror their non-lazy counterparts, by replacing eagerly constructed
`desc` fields by `lazy` values, that are computed on demand. This is
useful to explore large ASTs efficiently (note that Clang parsing
itself can still be slow; the lazy part only concerns the conversion
into the `Clang.Lazy.Ast` datatypes)


## Generating a new seed

Three files, `clang_stubs.c`, `clang__bindings.ml`, and
`clang__bindings.mli`, are generated for each version of LLVM by the
`stubgen` tool (sub-directory `tools/stubgen/`).

To generate these files for a given version of LLVM, you may run:
`stubgen --llvm-config=$PATH_TO_LLVM_CONFIG $TARGET_PATH`

`stubgen` depends on `pcre` and `cmdliner`.

Additionnally, three
files, `libclang_extensions_attrs.inc`,
`libclang_extensions_attrs_headers.inc`, and `attributes.ml` are
generated by the `generate_attrs` tool (sub-directory
`tools/generate_attrs`).

To generate these files, you may run
`generate_attrs --llvm-config=$PATH_TO_LLVM_CONFIG $PATH_TO_BOOTSTRAP_DIR`.
The tool enumerates all the attributes supported by the given version of Clang,
and uses the bootstrap directory both as target path and to determine in which
version of Clang each attribute has been introduced.
