open Sexplib
open Meta_conv.Types
open Meta_conv.Open

include S with type target = Sexp.t

module Default : sig
  val sexp_of_int       : int encoder
  val sexp_of_nativeint : nativeint encoder
  val sexp_of_unit      : unit encoder
  val sexp_of_bool      : bool encoder
  val sexp_of_int32     : int32 encoder
  val sexp_of_int64     : int64 encoder
  val sexp_of_float     : float encoder
  val sexp_of_char      : char encoder
  val sexp_of_string    : string encoder
  val sexp_of_list      : 'a encoder -> 'a list encoder
  val sexp_of_array     : 'a encoder -> 'a array encoder
  val sexp_of_option    : 'a encoder -> 'a option encoder
  (* val sexp_of_ref       : 'a encoder -> 'a ref encoder *)
  val sexp_of_lazy_t    : 'a encoder -> 'a Lazy.t encoder
  (* val sexp_of_mc_lazy_t : 'a encoder -> ('a, target) mc_lazy_t encoder *)
  val sexp_of_mc_fields : 'a encoder -> (string * 'a) list encoder
  (* val sexp_of_hashtbl : 'a encoder -> 'b encoder -> ('a, 'b) Hashtbl.t encoder *)
  val sexp_of_result    : 'a encoder -> 'b encoder -> ('a, 'b) result encoder

  val int_of_sexp       : int decoder
  val nativeint_of_sexp : nativeint decoder
  val unit_of_sexp      : unit decoder
  val bool_of_sexp      : bool decoder
  val int32_of_sexp     : int32 decoder
  val int64_of_sexp     : int64 decoder
  val float_of_sexp     : float decoder
  val char_of_sexp      : char decoder
  val string_of_sexp    : string decoder
  val list_of_sexp      : 'a decoder -> 'a list decoder
  val array_of_sexp     : 'a decoder -> 'a array decoder
  val option_of_sexp    : 'a decoder -> 'a option decoder
  (* val ref_of_sexp       : 'a decoder -> 'a ref decoder *)
  val lazy_t_of_sexp    : 'a decoder -> 'a lazy_t decoder
  val result_of_sexp    : 'a decoder -> 'b decoder -> ('a, 'b) result decoder
  
  val mc_lazy_t_of_sexp : 'a decoder -> ('a, Sexp.t) mc_lazy_t decoder  
  val mc_fields_of_sexp : 'a decoder -> (string * 'a) list decoder
  (* val hashtbl_of_sexp : 'a decoder -> 'b decoder -> ('a, 'b) Hashtbl.t decoder *)
end

include module type of Default
