open Meta_conv.Types
open Meta_conv.Open

include S with type target = Camlon.Ocaml.t

module Default : sig
  val ocaml_of_int       : int encoder
  val ocaml_of_nativeint : nativeint encoder
  val ocaml_of_unit      : unit encoder
  val ocaml_of_bool      : bool encoder
  val ocaml_of_int32     : int32 encoder
  val ocaml_of_int64     : int64 encoder
  val ocaml_of_float     : float encoder
  val ocaml_of_char      : char encoder
  val ocaml_of_string    : string encoder
  val ocaml_of_list      : 'a encoder -> 'a list encoder
  val ocaml_of_array     : 'a encoder -> 'a array encoder
  val ocaml_of_option    : 'a encoder -> 'a option encoder
  val ocaml_of_ref       : 'a encoder -> 'a ref encoder
  val ocaml_of_lazy_t    : 'a encoder -> 'a Lazy.t encoder
  val ocaml_of_mc_lazy_t : 'a encoder -> ('a, target) mc_lazy_t encoder
  val ocaml_of_mc_fields : 'a encoder -> (string * 'a) list encoder
  val ocaml_of_hashtbl   : 'a encoder -> 'b encoder -> ('a, 'b) Hashtbl.t encoder
  val ocaml_of_result    : 'a encoder -> 'b encoder -> ('a, 'b) result encoder

  val int_of_ocaml       : int decoder
  val nativeint_of_ocaml : nativeint decoder
  val unit_of_ocaml      : unit decoder
  val bool_of_ocaml      : bool decoder
  val int32_of_ocaml     : int32 decoder
  val int64_of_ocaml     : int64 decoder
  val float_of_ocaml     : float decoder
  val char_of_ocaml      : char decoder
  val string_of_ocaml    : string decoder
  val list_of_ocaml      : 'a decoder -> 'a list decoder
  val array_of_ocaml     : 'a decoder -> 'a array decoder
  val option_of_ocaml    : 'a decoder -> 'a option decoder
  val ref_of_ocaml       : 'a decoder -> 'a ref decoder
  val lazy_t_of_ocaml    : 'a decoder -> 'a lazy_t decoder
  val hashtbl_of_ocaml   : 'a decoder -> 'b decoder -> ('a, 'b) Hashtbl.t decoder
  val result_of_ocaml    : 'a decoder -> 'b decoder -> ('a, 'b) result decoder
  
  (*
  val mc_lazy_t_of_ocaml : 'a decoder -> ('a, target) mc_lazy_t decoder
  val mc_fields_of_ocaml : 'a decoder -> (string * 'a) list decoder
  *)
end

include module type of Default
