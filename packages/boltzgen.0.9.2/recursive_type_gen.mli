val evaluate : Type.sum_type -> unit
val named_of_string : string -> Type.named_type

val gen_string_of_compo : Type.recdefprint -> Type.compo_type -> string

val boltzman_from_compo : Type.compo_type -> Type.gen_function
val gen_from_compo :  Random.State.t -> int -> Type.compo_type -> float -> Type.hidden_type * int


val gen_random_fun_def :
  (Type.compo_type list * Type.compo_type, char) Hashtbl.t ->
  Type.func -> string

val call_random :
  ?tsrange:int * int ->
  ?max_iter:int ->
  (Type.compo_type list * Type.compo_type, char) Hashtbl.t ->
  Random.State.t -> int -> float -> Type.func -> string
