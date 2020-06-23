open Typerep_lib.Std

exception Overflow
exception Rounded

(** Between OCaml and Micheline *)

val of_micheline : 'a Typerep.t -> Michelson.Micheline.parsed -> 'a option
val to_micheline : 'a Typerep.t -> 'a -> Michelson.Micheline.t

(** Between OCaml and SCaml represetation of Michelson *)
val to_michelson : 'a Typerep.t -> 'a -> Michelson.Constant.t
val of_michelson : 'a Typerep.t -> Michelson.Constant.t -> 'a option

(** Between Micheline and SCaml representation of Michelson *)
val michelson_of_micheline : 'a Typerep.t -> Michelson.Micheline.parsed -> Michelson.Constant.t option
val micheline_of_michelson : Michelson.Constant.t -> Michelson.Micheline.t

(** Conversion from OCaml/SCaml type to Michelson type *)
val to_michelson_type : 'a Typerep.t -> Michelson.Type.t

