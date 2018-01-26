(** Signatures used in [Make.F] *)

module type Typemod = sig
  val type_implementation:
    string -> string -> string -> Env.t -> Parsetree.structure ->
    Typedtree.structure * Typedtree.module_coercion
  val type_interface:
    string -> Env.t -> Parsetree.signature -> Typedtree.signature
  val simplify_signature: Types.signature -> Types.signature
end

module type TypedTransformation = sig
  open Typedtree
  val map_structure : structure -> structure
  val map_signature : signature -> signature
end
  
