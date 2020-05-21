# 2020-05-15, 4.1.0

- Compatible with LLVM from 3.4.2 to 10.0.0, OCaml from 4.03.0 to 4.11.0

- Typeloc structures which contains non-transformed type informations can be
  obtained with `Clang.Decl.get_type_loc` and `Clang.Parameter.get_type_loc`.
  In particular, typeloc structures can be obtained to get the original size
  expression from constant arrays.
  (suggested by Damien Rouhling)

- `Clang.Expr.get_definition` and `Clang.Decl.get_canonical` to retrieve the cursors
  to the declaration site of identifiers, and `Clang.compare_cursors` for
  comparing cursors.
  (suggested by Arthur Charguéraud)

- Fix: Constant array sizes are no longer reported as default values for
  parameters.

- Fix: Correct representation of integer literal from 32 to 64 bits
  (reported by Hyunsoo Shin)

- Relies on `metapp`, `metaquot` and `refl`. All AST types are `reflexive` in
  the sense of the `refl` package.
  `ppxlib` and `override` are not dependencies anymore.

- `clangml.show` and `clangml.compare` packages don't exist anymore.
  Every AST node module (`Clang.Expr`, `Clang.Stmt`, ...) exposes a common
  signature (Clang.S) with `compare`, `equal`, `pp` and `show` functions, and
  `Set` and `Map` modules.

- Access to semantic form for initialization lists
  (suggested by Hyunsoo Shin)

- Representation for designated initialization expressions (C99)
  (suggested by Hyunsoo Shin)

- Fix: Some distributions such as ArchLinux now gather clang libraries in
  `-lclang-cpp`.
  (reported by Armaël Guéneau)

- libclang's tokenize and related functions are now accessible,
  new utility function `Clang.Expr.radix_of_integer_literal`.

- Fix: C stub generation used to declare CAMLlocal inside for loops, causing
  infinite loops in the GC in some occasions.
  (reported by Armaël Guéneau)

# 2019-09-30, 4.0.1

- Support for Clang/LLVM 9.0

- Compatible with OCaml 4.09

- Better representation for input and outputs in assembler statements

- u'c' syntax is now correctly supported with Clang 3.6 and 3.7

- Support for C++ exception specification, function try block

- Better support for Gentoo

- Bug fixes

# 2019-07-11, 4.0.0

- C++ support: all the code examples from C++14 and C++17 norms are covered
  by the AST.

- AST pretty-printers, comparators and lifters are now defined in sub-libraries
  (clangml.show, clangml.ord, clangml.lift). Visitors are no longer derived by
  default (`override` PPX library can be used to derive visitors from the AST
  if needed.)

- A very limited C/C++ pretty-printer is provided in the sub-library
  clangml.printer. It should be extended in next releases.

- Compatible with OCaml 4.08

- Bug fixes

- Flag dune as a build dependency
  (suggested by @kit-ty-kate in
   https://github.com/ocaml/opam-repository/pull/13434)

# 2019-02-11, 4.0.0beta1

- First public release
