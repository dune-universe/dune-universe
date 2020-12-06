(*Unfortunately Uint8 module does not define sexp converters, so we have to.*)
module Uint8_extended :
sig
  type t = Uint8.t
  type uint8 = t
  val zero : uint8
  val one : uint8
  val add : uint8 -> uint8 -> uint8
  val sub : uint8 -> uint8 -> uint8
  val mul : uint8 -> uint8 -> uint8
  val div : uint8 -> uint8 -> uint8
  val rem : uint8 -> uint8 -> uint8
  val succ : uint8 -> uint8
  val pred : uint8 -> uint8
  val max_int : uint8
  val min_int : uint8
  val logand : uint8 -> uint8 -> uint8
  val logor : uint8 -> uint8 -> uint8
  val logxor : uint8 -> uint8 -> uint8
  val lognot : uint8 -> uint8
  val shift_left : uint8 -> int -> uint8
  val shift_right : uint8 -> int -> uint8
  val of_int : int -> uint8
  val to_int : uint8 -> int
  val of_float : float -> uint8
  val to_float : uint8 -> float
  (*val of_int8 : int8 -> uint8
  val to_int8 : uint8 -> int8
  val of_nativeint : nativeint -> uint8
  val to_nativeint : uint8 -> nativeint
  val of_int8 : int8 -> uint8
  val to_int8 : uint8 -> int8*)
  val of_string : string -> uint8
  val to_string : uint8 -> string
  val to_string_bin : uint8 -> string
  val to_string_oct : uint8 -> string
  val to_string_hex : uint8 -> string
  val compare : t -> t -> int
  val printer : Format.formatter -> uint8 -> unit
  val printer_bin : Format.formatter -> uint8 -> unit
  val printer_oct : Format.formatter -> uint8 -> unit
  val printer_hex : Format.formatter -> uint8 -> unit
						    
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_uint8 : Uint8.t -> Sexplib.Sexp.t
  val uint8_of_sexp : Sexplib.Sexp.t -> Uint8.t
  val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val show : t -> Ppx_deriving_runtime.string
  val pp_uint8 : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val show_uint8 : t -> Ppx_deriving_runtime.string
  val equal_uint8 : Uint8.t -> Uint8.t -> bool
  val compare_uint8 : Uint8.t -> Uint8.t -> int
  val equal : t -> t -> bool
  (*the .json type gets changed to .t sometime after 4.06.0*)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
(* Only works with local hacked csvfields
  val to_xml : t -> Csvfields.Xml.xml list
  val of_xml : Csvfields.Xml.xml -> t
  val xsd : Csvfields.Xml.xml list*)
  module T2 : sig
    include Core.Comparable.S with type t := t
  end
end 
