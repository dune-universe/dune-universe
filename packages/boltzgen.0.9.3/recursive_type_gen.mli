(** Functions for handling value generation except sum types *)

val evaluate : Type.sum_type -> unit
(** Build the type and add it to the type librairy *)

val named_of_string : string -> Type.named_type
(** Parse and build a type definition *)

val gen_string_of_compo : Type.recdefprint -> Type.compo_type -> string
(** Generate the to_string function for a type *)

val boltzman_memoize : (Type.compo_type * float, float * float) Hashtbl.t

val add_memoize : Type.compo_type -> float -> float * float -> unit

val equations_from_compo : Type.compo_type -> Type.poly_assoc -> Type.poly_assoc
(** return all boltzmann equations *)

val boltzman_from_compo : Type.compo_type -> Type.gen_function
(** Evaluate the boltzmann function for a type *)

val gen_from_compo :
  Random.State.t -> int -> Type.compo_type -> float -> Type.hidden_type * int
(** Generate a value for a type *)

val print_from_compo :
  Type.compo_type -> Format.formatter -> Type.hidden_type -> unit
(** Print a value given its type *)

val gen_random_fun_def :
  Format.formatter ->
  (Type.compo_type list * Type.compo_type, char) Hashtbl.t ->
  Type.func ->
  unit
(** Generate and print random functions with memoisation *)

val call_random :
  ?tsrange:int * int ->
  ?max_iter:int ->
  (Type.compo_type list * Type.compo_type, char) Hashtbl.t ->
  Random.State.t ->
  int ->
  float ->
  Type.func ->
  string
(** Generate and print a call for a function *)
