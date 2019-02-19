# clangml: OCaml bindings for clang.

clangml provides bindings for all versions of clang, from 3.4 to
8.0.0.

## Introduction

This library is a complete rewritting of the previous clangml
(clangml versions <4.0.0):
the bindings now rely on automatically generated C stubs to libclang, with
some extensions when libclang is incomplete.
Contrary to old clangml versions, the versions of clangml from 4.0.0 are
independent from the version of the clang library:
any version of clangml from 4.0.0 can be built with any version of the
clang library in the supported interval.
Currently, all versions of clang, from 3.4 to 8.0.0, are supported.

However, clangml is statically linked to libclang, and clangml needs
to be rebuilt for every version of libclang to run with.
In addition, the low-level bindings are automatically generated
from libclang's header and their signature can change from one version
of libclang to another.

The high-level bindings ([`Clang.Ast`][4], [`Clang.Type`][7],
[`Clang.Expr`][8], [`Clang.Stmt`][9], [`Clang.Decl`][10]
and [`Clang.Enum_constant`][11]) provide abstractions
that are essentially independent from libclang version.
These abstraction aim mainly to provide an algebraic datatype
representation of Clang abstract syntax tree (AST).
It is worth noticing that there can be some differences in the way clang
parses file from one version to another (in particular, some features of the
C/C++ languages are only supported by recent versions of clang,
see some examples in [`Clang__ast`][19] module documentation).

## Installation

clangml is installable via `opam`:
```
opam install clangml
```

Manual installation requires a bootstrapped source directory.
Commits from branch `snapshot` are bootstrapped: a new snapshot
is committed by continuous integration after every successful build from
`master`.

Snapshot tarball:
https://gitlab.inria.fr/tmartine/clangml/-/archive/snapshot/clangml-snapshot.tar.gz

To build clangml from snapshot or from a bootstrapped source directory,
you may either:

* execute `./configure && make && make install`
(this method is recommended if you have to pass some options to configure);
* execute
`opam pin add git+https://gitlab.inria.fr/tmartine/clangml.git#snapshot`.

To bootstrap the repository from a development branch (e.g., `master`),
execute `./bootstrap.sh` first, then `./configure && make && make install` as
usual.

clangml's `configure` relies on `llvm-config` to find clang's library.
By default, `llvm-config` is searched in `PATH`, and you may
specify a path with `./configure --with-llvm-config=...`.

clangml requires some dependencies:
`opam install dune stdcompat ppx_deriving visitors`.
Additionnally, to run `make tests`: `opam install ocamlcodoc`.

`libclang` and other external dependencies can be installed with opam depext
plugin:
```
opam pin add -n git+https://gitlab.inria.fr/tmartine/clangml.git#snapshot
opam depext -i clangml
```
(`-n` option asks `opam pin` not to install clangml directly, and `-i` option
asks `opam depext` to install clangml once dependencies are installed.)

## Usage

The module [`Clang`][1] provides direct bindings to most of the [symbols
defined by libclang][2] to match OCaml conventions, camel-case symbols
have been renamed to lower-case symbols with underscores, and `clang_`
prefixes have been removed. Additional bindings have been defined in
[`libclang_extensions.h`][3] for some parts of clang's API that have
not been covered by libclang.

[1]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/index.html
[2]: https://clang.llvm.org/doxygen/group__CINDEX.html
[3]: https://gitlab.inria.fr/tmartine/clangml/blob/master/clangml/libclang_extensions.h

The module [`Clang.Ast`][4] provides a higher-level interface to clang's AST.
The function [`Clang.Ast.parse_file`][17] returns the AST from a file
and [`Clang.Ast.parse_string`][18] returns the AST from a string.
You may try these functions in OCaml toplevel to discover the resulting data
structure.

[17]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html#val-parse_file
[18]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html#val-parse_string

The module [`Clang.Ast`][4] includes in particular the module [`Clang__ast`][19]
which declares the algebraic data types that represent the AST.
The module [`Clang__ast`][19] uses [ppx_deriving][5] and [visitors][6] to make the
data structure comparable, showable and visitable.
The documentation of most of the nodes contains examples that can be used as references
for how syntactic constructions are parsed, and that are extracted with [ocamlcodoc][20]
and serve as unit tests with `dune runtest` (or, equivalently, `make tests`).

[19]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang__ast/index.html
[20]: https://gitlab.inria.fr/tmartine/ocamlcodoc

Modules [`Clang.Type`][7], [`Clang.Expr`][8], [`Clang.Stmt`][9],
[`Clang.Decl`][10] and [`Clang.Enum_constant`][11] provides sub-modules
`Set` and `Map` as well as high-level abstractions to some libclang's bindings.

[4]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Ast/index.html
[5]: https://github.com/ocaml-ppx/ppx_deriving
[6]: https://gitlab.inria.fr/fpottier/visitors
[7]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html
[8]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Expr/index.html
[9]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Stmt/index.html
[10]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Decl/index.html
[11]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Enum_constant/index.html

In particular:

- given an expression node `e : Clang.Expr.t`, the type of `e` can be obtained by [`Clang.Type.of_node e`][12];
- given a type `t : Clang.Type.t`, the alignment and the size of `t` can be obtained by [`Clang.Type.get_align_of t`][13] and [`Clang.Type.get_size_of t`][14] respectively;
- if `t : Clang.Type.t` is a `typedef`, the underlying type declared for `t` can be obtained by [`Clang.Type.get_typedef_underlying_type`][15]
- if `t : Clang.Type.t` is a record (`struct` or `union`), the list of fields can by [`Clang.Type.list_of_fields`][16];

[12]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-of_node
[13]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_align_of
[14]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_size_of
[15]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-get_typedef_underlying_type
[16]: https://tmartine.gitlabpages.inria.fr/clangml/doc/clangml/Clang/Type/index.html#val-list_of_fields

## Generating a new seed

Three files, `clang_stubs.c`, `clang__bindings.ml` and
`clang__bindings.mli`, are generated for each version of LLVM by the
`stubgen` tool (sub-directory stubgen).

To generate these files for a given version of LLVM, you may run:
`stubgen --llvm-config=$PATH_TO_LLVM_CONFIG $TARGET_PATH`.

`stubgen` depends on `pcre` and `cmdliner`.