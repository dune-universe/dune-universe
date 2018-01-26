# TyPPX: Library for PPX with Types

TyPPX is a library to build PPX'es with types.  Typing the input Parsetree
by the vanilla or modified OCaml compiler's type inference, you can perform
AST transformation with type information over Typedtree.

PPX is transformation of the untyped syntax tree (Parsetree). The input and
the output must be both in Parsetree. But inside it you can run OCaml type checker
possibly with some modifications in it and perform transformation of the typed
syntax tree (Typedtree) using type information.  TyPPX provides a functor
which takes AST mappers and typing module then returns a function to run
the entire PPX work.

# Use of PPX with Types

TyPPX enables to use type information in OCaml pre-processing.  Some compiler
modifications which was used to be patches can be implemented as PPX
preprocessors with TyPPX.  This makes the delivery of modifications to users
very easy: users do not need to switch their compilers but simply can use PPX
with their current compilers.

# Patching vs PPX with types

PPX with types are not usable for all the compiler modifications:

* You cannot change the syntax.  Attributes like `[@blah]` can be used to have special nodes though.
* You cannot change the type algebra (cmi format).  You can still change the type inference algorithm though.
* You cannot change the code generation.  Your changes of program behaviour must be encoded as the vanilla OCaml code.

With these limitations PPX with types has the following benefits:

* No need of compiler patching, which means users can try your modifications pretty easily.
* Output of PPX is typed again by the vanilla OCaml compiler.  This is much more secure than the direct compiler patching where bugs often lead to program crashes (segmentation faults).  The output of PPX is also printable as OCaml code therefore debugging is much easier.
* Good at quick prototyping for compiler patch.  Once your modification is successful as a PPX with types, conversion to a compiler patch is easy: you can reuse much of the PPX code for the patch.

# Phases of TyPPX

## First untyped transformation of Parsetree.

Parsed code is first passed to "first untyped transformation".
The mapper of this phase is given by `firstUntypedTransformation : Ast_mapper.mapper`.
The mapper is responsible to convert the input Parsetree to a Parsetree
which can be typable by the next typing phase.

## Typing

The output of the first untyped transformation is given to a type inference.
This type inference can be either from the vanilla OCaml or from a modified
version of it.  The typing algorithm should be given as `Typemod` module
in the functor argument.

## Typed transformation of Typedtree

The output of the typing phase is then transformed by the typed transformation,
which is given by `TypedTransformation : S.TypedTransformation` module. Here
you can perform program transformation using type information obtained from
the typing.

The output of this phase is Typedtree.  The result is untyped, then will be
type-checked again by the vanilla OCaml compiler, therefore it must be
well-typed in the vanilla OCaml.  However, the transformation needs not to
create correctly type-annotated Typedtree, since the attached type information
is removed anyway by the next untyping phase.

## Untyping

The output Typedtree of the typed transformation is coerced back to Parsetree.

## Last untyped transformation of Parsetree

You may perform the "last untyped transformation" against the output of
the untyping.  The mapper of this phase is given by `lastUntypedTransformation : Ast_mapper.mapper`.  The output of this phase is the final result of the entire
PPX.

# Limitations

## The input of TyPPX based PPX preprocessor must be typable.

The input of TyPPX are type-checked.  Therefore the input cannot contain
extensions (`[%...]` and `[%%...]`). These extensions must be expanded
by other PPX preprocessors before the input is given to TyPPX based
PPX preprocessor.

## TyPPX doesn't work with REPL (`ocaml`)

PPX with types and TyPPX do not work with OCaml toplevel (REPL).
This is due to the architecture of PPX preprocessor:

* PPX preprocessor command is executed for each compilation unit.
* In OCaml toplevel, each toplevel phrase is a compilation unit.
* PPX command invocations cannot carry over huge states like the typing environment.

# Build

## Requirements

* OCaml 4.02.3 and later
* ocamlfind
* omake

## Install

* `ocaml setup.ml -configure`
* `ocaml setup.ml -build`
* `ocaml setup.ml -install`

