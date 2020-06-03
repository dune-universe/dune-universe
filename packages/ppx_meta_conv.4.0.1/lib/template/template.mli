open Meta_conv.Types
open Meta_conv.Open
open Foobar 
(** You may want to open the module for the target data. *)

(** The module Foobar *must* define the target type as [t]. *)

include S with type target = Foobar.t
(** You must define the primitive coding module of type [S with type target = Foobar.t] 

    This is created by

    [include Meta_conv.Internal.Make(struct
      ...
      end)]

    where the argument module has the module type [Types.Min with type target = Foobar.t].
*)

(** Encoders of primitive types. 
    You do not need to implemente all of them, 
    but auto-generation of derived type encoders depending 
    on unimplemented primitve encoders fail.

    There are helper functions in Internal to implement these encoders,
    see [Helper.<tycon>_of] functions.
*)
val foobar_of_int       : int encoder
val foobar_of_nativeint : nativeint encoder
val foobar_of_unit      : unit encoder
val foobar_of_bool      : bool encoder
val foobar_of_int32     : int32 encoder
val foobar_of_int64     : int64 encoder
val foobar_of_float     : float encoder
val foobar_of_char      : char encoder
val foobar_of_string    : string encoder
val foobar_of_list      : 'a encoder -> 'a list encoder
val foobar_of_array     : 'a encoder -> 'a array encoder
val foobar_of_option    : 'a encoder -> 'a option encoder
val foobar_of_lazy_t    : 'a encoder -> 'a Lazy.t encoder

(** Decoders of primitive types. 
    You do not need to implemente all of them, 
    but auto-generation of derived type decoders depending 
    on unimplemented primitve decoders fail.

    There are helper functions in Internal to implement these decoders,
    see [Helper.of_<tycon>] functions.
*)
val int_of_foobar       : int decoder
val nativeint_of_foobar : nativeint decoder
val unit_of_foobar      : unit decoder
val bool_of_foobar      : bool decoder
val int32_of_foobar     : int32 decoder
val int64_of_foobar     : int64 decoder
val float_of_foobar     : float decoder
val char_of_foobar      : char decoder
val string_of_foobar    : string decoder
val list_of_foobar      : 'a decoder -> 'a list decoder
val array_of_foobar     : 'a decoder -> 'a array decoder
val option_of_foobar    : 'a decoder -> 'a option decoder
val lazy_t_of_foobar    : 'a decoder -> 'a lazy_t decoder

(** Encoder and Decoder of Hashtbl.t 

    They can be defined using the helper functions [Helper.of_hashtbl] 
    and [Helper.hashtbl_of] using [('a * 'b) list] as actual encoding/decoding. 

    Note that you must use the type name [hashtbl] instead of [Hashtbl.t],
    unless somehow you extend [Hashtbl] module with [Hashtbl.foobar_of_t]
    and [Hashtbl.t_of_foobar]. The alias of [hashtbl] is available in module [Open].
*)
val foobar_of_hashtbl   : 'a encoder -> 'b encoder -> ('a, 'b) Hashtbl.t encoder
val hashtbl_of_foobar   : 'a decoder -> 'b decoder -> ('a, 'b) Hashtbl.t decoder

(** Encoders of special types *)
val foobar_of_mc_lazy_t : 'a encoder -> ('a, foobar) mc_lazy_t encoder
val foobar_of_mc_fields : 'a encoder -> (string * 'a) list encoder

(** Decoders of special types *)
val mc_lazy_t_of_foobar : 'a decoder -> ('a, foobar) mc_lazy_t decoder
val mc_fields_of_foobar : 'a decoder -> (string * 'a) list decoder
