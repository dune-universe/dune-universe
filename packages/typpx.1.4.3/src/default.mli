val untyped_identity : Ast_mapper.mapper
(** The identity of untyped transformation.

    Note that this is not equivalent with [Ast_mapper.default_mapper]
    but returns the input structure ans signature themselves immediately 
    without any recursive traversal.
 *)

module Typemod : S.Typemod
(** The vanilla OCaml's type inference algorithm *)

module Typed_identity : S.TypedTransformation
(** The identity of typed transformation *)
