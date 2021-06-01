val named_of_sum : Type.sum_type -> Type.named_type

val print_equations :
  Format.formatter -> (Type.compo_type * Type.poly) array -> unit

(*val compute_jacobian :
  (Type.compo_type * Type.poly) array -> Type.poly array array

val ap_poly : (Type.compo_type * int) list -> Type.poly -> float array -> float

val map_of_equations :
  (Type.compo_type * Type.poly) array -> (Type.compo_type * int) list

val comp_dx :
  (Type.compo_type * Type.poly) array ->
  Type.poly array array ->
  (Type.compo_type * int) list ->
  float array ->
  float array * float array
*)

val compute_size :
  (Type.compo_type -> float -> (float * float) option) ->
  Type.poly_assoc ->
  float ->
  (Type.compo_type * float * float) list option
