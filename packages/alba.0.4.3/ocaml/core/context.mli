open Fmlib


type t

val count: t -> int

val gamma: t -> Gamma.t
val name_map: t -> Name_map.t
val index_of_level: int -> t -> int
val level_of_index: int -> t -> int


val empty: t

val find_name: string -> t -> int list

val compute: Term.t -> t -> Term.t

val push_local: string -> Term.typ -> t -> t

val can_add_global: string -> Term.typ -> t -> bool

val add_axiom: string -> Term.typ -> t -> t

val add_builtin_type:     string -> string -> Term.typ -> t -> t
val add_builtin_function: string -> string -> Term.typ -> t -> t

val add_definition: string -> Term.typ -> Term.t -> t -> (t, int) result

val add_inductive: Inductive.t -> t -> t


module Pretty (P:Pretty_printer.SIG):
sig
  val print: Term.t -> t -> P.t
end
