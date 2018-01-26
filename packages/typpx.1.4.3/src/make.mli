module F(A : sig
  val tool_name : string
  (** Name of PPX *)
    
  val args : (Arg.key * Arg.spec * Arg.doc) list
  (** Command line arguments.

      Actual command line processing is done with this [args] plus typpx's 
      default options which include compiler related and -debug.

      For the types of Arg, see Arg module for details. 
  *)
    
  val firstUntypedTransformation : Ast_mapper.mapper
  (** Mapper for the first untyped AST transformation *)

  module Typemod : S.Typemod
  (** Typing *)

  module TypedTransformation : S.TypedTransformation
  (** Mapper for the typed AST transformation *)

  val lastUntypedTransformation : Ast_mapper.mapper
  (** Mapper for the last untyped AST transformation *)
    
end) : sig
  (** Runs PPX with type, using the above transformations.
      Conversion phases are as follows:

      * [firstUntypedTransformation] of Parsetree
      * Typing by [Typemod], conversion from Parsetree to Typedtree
      * [TypedTransformation] of Typedtree
      * Untyping by [Untypeast] module, back to Parsetree
      * [lastUntypedTransformation] of Parsetree
  *)

  val legacy_main : unit -> unit
  val register : unit -> unit
end
