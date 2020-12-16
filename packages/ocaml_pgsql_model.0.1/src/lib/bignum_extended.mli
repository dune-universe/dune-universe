module Bignum_extended : sig
  (*type t = Bignum.t*)
  include (module type of Bignum)
  
  val show : t -> Ppx_deriving_runtime.string
  val pp : Format.formatter -> t -> Ppx_deriving_runtime.unit
  val to_string : t -> string
  val compare : t -> t -> Ppx_deriving_runtime.int
  val equal : t -> t -> Ppx_deriving_runtime.bool
  (*the .json type gets changed to .t sometime after 4.06.0*)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

  (*val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t*)

  (* Only usefule with a local hacked version of csvfields
  val to_xml : t -> Csvfields.Xml.xml list
  val of_xml : Csvfields.Xml.xml -> t
  val xsd : Csvfields.Xml.xml list*)
end
