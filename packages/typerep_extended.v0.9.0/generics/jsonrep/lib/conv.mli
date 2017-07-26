open! Core
open Json_wheel_jane_street_overlay.Std

exception Type_mismatch of string * Json_type.t

val int_of_json : Json_type.t -> int
val int32_of_json : Json_type.t -> int32
val int64_of_json : Json_type.t -> int64
val nativeint_of_json : Json_type.t -> nativeint
val char_of_json : Json_type.t -> char
val float_of_json : Json_type.t -> float
val string_of_json : Json_type.t -> string
val bool_of_json : Json_type.t -> bool
val unit_of_json : Json_type.t -> unit
val option_of_json : (Json_type.t -> 'a) -> Json_type.t -> 'a option
val list_of_json : (Json_type.t -> 'a) -> Json_type.t -> 'a list
val array_of_json : (Json_type.t -> 'a) -> Json_type.t -> 'a array
val lazy_t_of_json : (Json_type.t -> 'a) -> Json_type.t -> 'a lazy_t
val function_of_json :
  (Json_type.t -> 'a)
  -> (Json_type.t -> 'b)
  -> Json_type.t -> ('a -> 'b)
val ref_of_json : (Json_type.t -> 'a) -> Json_type.t -> 'a ref
val tuple2_of_json :
  (Json_type.t -> 'a)
  -> (Json_type.t -> 'b)
  -> Json_type.t -> ('a * 'b)
val tuple3_of_json :
  (Json_type.t -> 'a)
  -> (Json_type.t -> 'b)
  -> (Json_type.t -> 'c)
  -> Json_type.t -> ('a * 'b * 'c)
val tuple4_of_json  :
  (Json_type.t -> 'a)
  -> (Json_type.t -> 'b)
  -> (Json_type.t -> 'c)
  -> (Json_type.t -> 'd)
  -> Json_type.t -> ('a * 'b * 'c * 'd)
val tuple5_of_json  :
  (Json_type.t -> 'a)
  -> (Json_type.t -> 'b)
  -> (Json_type.t -> 'c)
  -> (Json_type.t -> 'd)
  -> (Json_type.t -> 'e)
  -> Json_type.t -> ('a * 'b * 'c * 'd * 'e)

val json_of_int : int -> Json_type.t
val json_of_int32 : int32 -> Json_type.t
val json_of_int64 : int64 -> Json_type.t
val json_of_nativeint : nativeint -> Json_type.t
val json_of_char : char -> Json_type.t
val json_of_float : float -> Json_type.t
val json_of_string : string -> Json_type.t
val json_of_bool : bool -> Json_type.t
val json_of_unit : unit -> Json_type.t

val json_of_option : ('a  -> Json_type.t) -> 'a option -> Json_type.t
val json_of_list : ('a  -> Json_type.t) -> 'a list -> Json_type.t
val json_of_array : ('a  -> Json_type.t) -> 'a array -> Json_type.t
val json_of_lazy_t : ('a  -> Json_type.t) -> 'a lazy_t -> Json_type.t
val json_of_ref : ('a  -> Json_type.t) -> 'a ref -> Json_type.t

val json_of_function :
  ('a  -> Json_type.t) ->
  ('b  -> Json_type.t) ->
  ('a -> 'b) -> Json_type.t

val json_of_tuple2 :
  ('a -> Json_type.t) -> ('b -> Json_type.t)
  -> ('a * 'b) -> Json_type.t
val json_of_tuple3 :
  ('a -> Json_type.t) -> ('b -> Json_type.t)
  -> ('c -> Json_type.t)
  -> ('a * 'b * 'c) -> Json_type.t
val json_of_tuple4 :
  ('a -> Json_type.t) -> ('b -> Json_type.t)
  -> ('c -> Json_type.t) -> ('d -> Json_type.t)
  -> ('a * 'b * 'c * 'd) -> Json_type.t
val json_of_tuple5 : ('a -> Json_type.t) -> ('b -> Json_type.t)
  -> ('c -> Json_type.t) -> ('d -> Json_type.t) -> ('e -> Json_type.t)
  -> ('a * 'b * 'c * 'd * 'e) -> Json_type.t
