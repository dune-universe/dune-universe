(*Unfortunately Uint32 module does not define sexp converters, so we have to.*)
module Uint32_extended :
sig
  type t = Uint32.t
  type uint32 = t
  val zero : uint32
  val one : uint32
  val add : uint32 -> uint32 -> uint32
  val sub : uint32 -> uint32 -> uint32
  val mul : uint32 -> uint32 -> uint32
  val div : uint32 -> uint32 -> uint32
  val rem : uint32 -> uint32 -> uint32
  val succ : uint32 -> uint32
  val pred : uint32 -> uint32
  val max_int : uint32
  val min_int : uint32
  val logand : uint32 -> uint32 -> uint32
  val logor : uint32 -> uint32 -> uint32
  val logxor : uint32 -> uint32 -> uint32
  val lognot : uint32 -> uint32
  val shift_left : uint32 -> int -> uint32
  val shift_right : uint32 -> int -> uint32
  val of_int : int -> uint32
  val to_int : uint32 -> int
  val of_float : float -> uint32
  val to_float : uint32 -> float
  val of_string : string -> uint32
  val to_string : uint32 -> string
  val to_string_bin : uint32 -> string
  val to_string_oct : uint32 -> string
  val to_string_hex : uint32 -> string
  val compare : t -> t -> int
  val printer : Format.formatter -> uint32 -> unit
  val printer_bin : Format.formatter -> uint32 -> unit
  val printer_oct : Format.formatter -> uint32 -> unit
  val printer_hex : Format.formatter -> uint32 -> unit
						    
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_uint32 : Uint32.t -> Sexplib.Sexp.t
  val uint32_of_sexp : Sexplib.Sexp.t -> Uint32.t
  val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val show : t -> Ppx_deriving_runtime.string
  val pp_uint32 : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val show_uint32 : t -> Ppx_deriving_runtime.string
  val equal_uint32 : Uint32.t -> Uint32.t -> bool
  val compare_uint32 : Uint32.t -> Uint32.t -> int
  val equal : t -> t -> bool
  (*the .json type gets changed to .t sometime after 4.06.0*)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

(*Only works with a hacked version of csvfields
  val to_xml : t -> Csvfields.Xml.xml list
  val of_xml : Csvfields.Xml.xml -> t
  val xsd : Csvfields.Xml.xml list*)
  module T2 : sig
    include Core.Comparable.S with type t := t
  end
end 
