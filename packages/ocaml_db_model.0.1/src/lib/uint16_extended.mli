(*Unfortunately Uint16 module does not define sexp converters, so we have to.*)
module Uint16_extended :
sig
  type t = Uint16.t
  type uint16 = t
  val zero : uint16
  val one : uint16
  val add : uint16 -> uint16 -> uint16
  val sub : uint16 -> uint16 -> uint16
  val mul : uint16 -> uint16 -> uint16
  val div : uint16 -> uint16 -> uint16
  val rem : uint16 -> uint16 -> uint16
  val succ : uint16 -> uint16
  val pred : uint16 -> uint16
  val max_int : uint16
  val min_int : uint16
  val logand : uint16 -> uint16 -> uint16
  val logor : uint16 -> uint16 -> uint16
  val logxor : uint16 -> uint16 -> uint16
  val lognot : uint16 -> uint16
  val shift_left : uint16 -> int -> uint16
  val shift_right : uint16 -> int -> uint16
  val of_int : int -> uint16
  val to_int : uint16 -> int
  val of_float : float -> uint16
  val to_float : uint16 -> float
  val of_string : string -> uint16
  val to_string : uint16 -> string
  val to_string_bin : uint16 -> string
  val to_string_oct : uint16 -> string
  val to_string_hex : uint16 -> string
  val compare : t -> t -> int
  val printer : Format.formatter -> uint16 -> unit
  val printer_bin : Format.formatter -> uint16 -> unit
  val printer_oct : Format.formatter -> uint16 -> unit
  val printer_hex : Format.formatter -> uint16 -> unit
						    
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_uint16 : Uint16.t -> Sexplib.Sexp.t
  val uint16_of_sexp : Sexplib.Sexp.t -> Uint16.t
  val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val show : t -> Ppx_deriving_runtime.string
  val pp_uint16 : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val show_uint16 : t -> Ppx_deriving_runtime.string
  val equal_uint16 : Uint16.t -> Uint16.t -> bool
  val compare_uint16 : Uint16.t -> Uint16.t -> int
  val equal : t -> t -> bool
  (*the .json type gets changed to .t sometime after 4.06.0*)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
(* Only works with a hacked version of csvfields
  val to_xml : t -> Csvfields.Xml.xml list
  val of_xml : Csvfields.Xml.xml -> t
  val xsd : Csvfields.Xml.xml list*)
  module T2 : sig
    include Core.Comparable.S with type t := t
  end
end 
