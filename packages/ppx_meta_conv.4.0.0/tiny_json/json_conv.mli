open Tiny_json
open Meta_conv.Open

include Meta_conv.Types.S with type target = Json.t

module Default : sig
  val json_of_int       : int encoder
  val json_of_nativeint : nativeint encoder
  val json_of_unit      : unit encoder
  val json_of_bool      : bool encoder
  val json_of_int32     : int32 encoder
  val json_of_int64     : int64 encoder
  val json_of_float     : float encoder
  val json_of_char      : char encoder
  val json_of_string    : string encoder
  val json_of_list      : 'a encoder -> 'a list encoder
  val json_of_array     : 'a encoder -> 'a array encoder
  val json_of_option    : 'a encoder -> 'a option encoder
  val json_of_lazy_t    : 'a encoder -> 'a Lazy.t encoder
  (* val json_of_mc_lazy_t : 'a encoder -> ('a, target) mc_lazy_t encoder *)
  val json_of_mc_fields : 'a encoder -> (string * 'a) list encoder
  
  val int_of_json       : int decoder
  val nativeint_of_json : nativeint decoder
  val unit_of_json      : unit decoder
  val bool_of_json      : bool decoder
  val int32_of_json     : int32 decoder
  val int64_of_json     : int64 decoder
  val float_of_json     : float decoder
  val char_of_json      : char decoder
  val string_of_json    : string decoder
  val list_of_json      : 'a decoder -> 'a list decoder
  val array_of_json     : 'a decoder -> 'a array decoder
  val option_of_json    : 'a decoder -> 'a option decoder
  val lazy_t_of_json    : 'a decoder -> 'a lazy_t decoder
  val mc_lazy_t_of_json : 'a decoder -> ('a, target) mc_lazy_t decoder
  val mc_fields_of_json : 'a decoder -> (string * 'a) list decoder
end

include module type of Default
