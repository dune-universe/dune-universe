(* Call the compiler lib to parse the string as a signature, then convert it to Type *)

val parse_string :
  string -> Type.sum_type list * Type.func
val parse_typedef :
  string -> Type.sum_type
