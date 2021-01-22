val print_poly : out_channel -> (int * float * 'a) list -> unit

val dichotomie :
  ?factor:float ->
  ?up_bound:bool ->
  float * float ->
  (float -> float * float) ->
  float ->
  float

exception Zero_derivative

val newton_raphson_iterate :
  ?factor:float ->
  ?max_iter:int ->
  ?bound:float * float ->
  (float -> float * float) ->
  float ->
  float

val named_of_sum : Type.sum_type -> Type.named_type

val geom_law : float -> float -> int
