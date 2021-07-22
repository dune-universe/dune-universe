type t

type error =
  [ `Invalid_data_block_count
  | `Invalid_drop_count
  | `Invalid_systematic_scaling_factor
  ]

val systematic : t -> bool

val systematic_scaling_factor : t -> float

val data_block_count : t -> int

val max_drop_count : t -> int

val dist : t -> Dist.t

val make :
  ?systematic_scaling_factor:float ->
  systematic:bool ->
  data_block_count:int ->
  max_drop_count:int ->
  unit ->
  (t, error) result
