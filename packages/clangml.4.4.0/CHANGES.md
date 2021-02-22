# 2021-02-21, 4.4.0

- Support for Clang/LLVM 11.0.1, 11.1.0 and 12.0.0.

- Support for OCaml 4.12.

- Fix integer overflow on 32-bit platforms.

# 2020-09-22, 4.3.0

- Port to ppxlib 0.16 / ocaml-migrate-parsetree 2.0.0

- Support for Clang/LLVM 11.0.0

- New module `Clang.Cursor`, exposing the signature
  `Hashtbl.HashedType with type t = cxcursor`
  and `Clang.Cursor.Hashtbl`.

- New function `Clang.Decl.annotate_access_specifier`, to enumerate
  class fields with their access specifiers.

- Fix segmentation fault when parsing the redeclaration of a function decl
  without prototype when the previous declarations has some parameters.
  (reported by Kihong Heo)

- New field `has_written_prototype` in `function_decl` to distinguish
  between function declarations where the prototype is actually provided
  and declarations where the prototype is inherited.

# 2020-07-15, 4.2.0

- Support for Clang/LLVM 10.0.1

## C/C++ AST features

- Support for all C/C++ Clang attributes
  (including `_Alignas` (C) and `alignas` (C++) attributes, that have been
  requested by Damien Rouhling).
  See the auto-generated `bootstrap/attributes.ml`.

- class base specifiers now expose a full `qual_type` instead of a mere
  `ident` string.
  
- `ident_ref` now carries a `template_arguments` parameter.

- `function_decl` now exposes `inline_specified` and `inlined` fields.

- `function_type` now exposes `ref_qualifier` field.

- `TypeLoc` are used broaderly to compute `qual_type`: in particular,
  parameters in function types have now correct names.
  The type `type_loc` and the functions `Clang.Decl.get_type_loc` and
  `Clang.Parameter.get_type_loc` are kept for compatibility, but
  `qual_type` should contain all the informations.

## API changes

- `Clang.Lazy` exposes a lazy AST, that is to say that each `desc` field
  is a lazy value that is computed on demand. This is useful to explore
  large ASTs efficiently (note that Clang parsing itself can still be slow;
  the lazy part only concerns the conversion into the `Clang.Lazy.Ast`
  datatypes). Modules `Clang.Ast`, `Clang.Expr`, `Clang.Stmt`,
  `Clang.Type`, ..., have their lazy counter-parts: `Clang.Lazy.Ast`,
  `Clang.Lazy.Expr`, `Clang.Lazy.Stmt`, `Clang.Lazy.Type`, etc.

- The common signature exposed by every AST node module
  (`Clang.Expr`, `Clang.Stmt`, ...) now declares a `hash` function and
  and a `Hashtbl` module.

- `Clang.format_diagnostics` now prints presumed locations by default
  and the behavior can be changed through the optional `?options`
  argument. The implementation now relies on the new
  `Clang.pp_diagnostic` which is an OCaml reimplementation of
  `clang_formatDiagnostic` from libclang.

- `Clang.Expr.parse_string` parses strings as C expressions.
  (suggested by Damien Rouhling)

- `Clangml_printer` has now moved to `Clang.Printer`. The package
  `clangml.printer` still exists for compatibility and reexposes the
  `Clang.Printer` interface.

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
