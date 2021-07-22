(** {1 Constants}  *)

val max_data_block_count : int

val max_drop_count : int

(** {1 Basic types} *)

module Param : sig
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

  val make :
    ?systematic_scaling_factor:float ->
    systematic:bool ->
    data_block_count:int ->
    max_drop_count:int ->
    unit ->
    (t, error) result
end

type drop

val data_of_drop : drop -> Cstruct.t

module Drop_set : Set.S with type elt = drop

(** {1 Encoding} *)

type encode_error =
  [ `Inconsistent_data_block_size
  | `Invalid_drop_count
  | `Invalid_data_block_count
  | `Invalid_systematic_scaling_factor
  | `Invalid_drop_data_buffer
  ]

(** {2 Basic} *)

val encode :
  ?systematic_scaling_factor:float ->
  ?systematic:bool ->
  ?drop_data_buffer:Cstruct.t array ->
  max_drop_count:int ->
  Cstruct.t array ->
  (Param.t * drop array, encode_error) result

(** {2 Advanced} *)

type encoder

val create_encoder :
  ?drop_data_buffer:Cstruct.t array ->
  Param.t ->
  Cstruct.t array ->
  (encoder, encode_error) result

val reset_encoder : encoder -> unit

val param_of_encoder : encoder -> Param.t

val encoder_is_systematic : encoder -> bool

val data_block_count_of_encoder : encoder -> int

val max_drop_count_of_encoder : encoder -> int

val data_block_size_of_encoder : encoder -> int

val data_blocks_of_encoder : encoder -> Cstruct.t array

val encode_one : encoder -> drop option

val encode_all : encoder -> drop array

(** {1 Decoding} *)

type decode_error =
  [ `Invalid_drop_index
  | `Invalid_drop_degree
  | `Invalid_drop_count
  | `Invalid_data_block_buffer
  | `Invalid_data_block_size
  | `Invalid_drop_size
  | `Cannot_recover
  ]

(** {2 Basic} *)

val decode :
  ?data_block_buffer:Cstruct.t array ->
  Param.t ->
  Drop_set.t ->
  (Cstruct.t array, decode_error) result

(** {2 Advanced} *)

type decoder

val create_decoder :
  ?data_block_buffer:Cstruct.t array ->
  data_block_size:int ->
  Param.t ->
  (decoder, decode_error) result

val reset_decoder : decoder -> unit

val param_of_decoder : decoder -> Param.t

val decoder_is_systematic : decoder -> bool

val data_block_count_of_decoder : decoder -> int

val max_drop_count_of_decoder : decoder -> int

val data_block_size_of_decoder : decoder -> int

val drop_fill_count_of_decoder : decoder -> int

val data_blocks_of_decoder : decoder -> Cstruct.t array option

type decode_status =
  [ `Success of Cstruct.t array
  | `Ongoing
  ]

val decode_one : decoder -> drop -> (decode_status, decode_error) result

val decode_all : decoder -> Drop_set.t -> (Cstruct.t array, decode_error) result
