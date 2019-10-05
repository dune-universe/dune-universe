open Base

type 'sort decoding_error =
  [ `Wrong_value_sort_for_string_field of 'sort * string Field_value.typ
  | `Wrong_value_sort_for_int_field of 'sort * int Field_value.typ
  | `Wrong_value_sort_for_float_field of 'sort * float Field_value.typ
  | `Wrong_value_sort_for_bool_field of 'sort * bool Field_value.typ
  | `Wrong_value_sort_for_user_field of 'sort
  | `Wrong_value_sort_for_enum_field of 'sort
  | `Unrecognized_enum_value
  | `Multiple_oneof_fields_set
  | `Integer_outside_int_type_range of int64 ]

type 'v value = 'v Field_value.t

type 'v typ = 'v Field_value.typ

module type Encoding = sig
  type t

  type sort

  val encode_string : string value -> t

  val decode_string : string typ -> t -> (string, [> sort decoding_error]) Result.t

  val encode_int : int value -> t

  val decode_int : int typ -> t -> (int, [> sort decoding_error]) Result.t

  val encode_float : float value -> t

  val decode_float : float typ -> t -> (float, [> sort decoding_error]) Result.t

  val encode_bool : bool value -> t

  val decode_bool : bool typ -> t -> (bool, [> sort decoding_error]) Result.t
end
