(** Inferring types of input and output *)

open Common

val derive : mapping -> Tjson.t -> Query.t * input_vars * (string -> string) * (string * string * string * Tjson.t option)

val output : init:Atd.Ast.full_module -> mapping -> Tjson.t -> Atd.Ast.full_module

val print_reflect : string -> Yojson.Basic.t -> unit
