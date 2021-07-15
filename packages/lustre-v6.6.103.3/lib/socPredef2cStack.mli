(* Time-stamp: <modified the 04/02/2015 (at 14:38) by Erwan Jahier> *)


(** Returns the C code corresponding a soc key  *)
val get_predef_op: Soc.key -> string

(** Returns the C code implementing an iterator (map, fill, red) *)
val get_iterator : Soc.t -> string ->  Soc.t ->  int -> string


(** Returns the C code implementing a condact *)
val get_condact : Soc.t -> Soc.t -> Soc.var_expr list -> string 

(** Returns the C code implementing a boolred *)
val get_boolred : Soc.t -> int -> int -> int -> string 
