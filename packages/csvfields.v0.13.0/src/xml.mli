open! Core

(** Automatic conversion of OCaml field types into XML. This is used for excel
communication functions *)

(** Abstract representation of the xml type *)
type xml

(** The functions provided by the with xml camlp4 extension, and that need to be provided
 in a hand made conversion to be used by the extension. *)
module type Xmlable = sig
  type t
  val xsd : xml list
  val to_xml : t -> xml list
  val of_xml : xml -> t
end

(** Basic conversions *)
val to_string : xml -> string
val to_string_fmt : xml -> string
val to_human_string : xml -> string

module Parser_state : sig
  type t
  val make : unit -> t
end

(** Thread safe provided each thread uses a different [Parser_state.t] *)
val stateful_of_string : Parser_state.t -> string -> xml

val of_file : string -> xml

(** Basic traversal *)
val tag : xml -> string option
val attributes : xml -> (string * string) list
val children : xml -> xml list
val contents : xml -> string option
val child : xml -> string -> xml option
val kind : xml -> [ `Leaf | `Internal ]
val xml_data : string -> xml

(** Exceptions that could be raised by to_xml and of_xml *)
exception Illegal_atom of xml

exception Unexpected_xml of (xml * string)

(** @raise Unexpected_xml, Illegal_atom
    Used by the with xml extension
*)
val check_extra_fields : xml -> string list -> unit

(** XSD definition functions *)

(** An xsd complexType. *)
val complex_type : xml list -> xml
val decomplexify : xml list -> xml
val decomplexify_opt : xml list -> xml option
val decomplexify_list : xml list -> xml list option
val type_of_simple : xml list -> string

(** Standard wrapping to generate the necessary namespaces that are used in the automated
conversions *)
val wrap : xml -> xml

(** Restriction generation *)
module Restriction : sig
  type t
  module Format : sig

    (** Format restrictions in an xml atom *)
    type t =
      [ `string
      | `decimal
      | `date
      | `datetime
      | `time
      | `integer ]

    val of_string : string -> t
  end

  val enumeration     : string -> t
  val fraction_digits : int -> t
  val length          : int -> t
  val max_exclusive   : int -> t
  val min_exclusive   : int -> t
  val max_inclusive   : int -> t
  val min_inclusive   : int -> t
  val min_length      : int -> t
  val max_length      : int -> t
  val pattern         : string -> t
  val total_digits    : int -> t

  (** An xsd simpleType *)
  val simple_type :
    restrictions:t list
    -> format:Format.t
    -> xml list
end

val xsd_element : ?attr:(string * string) list ->  name:string -> xml list -> xml
val xml_element : ?attr:(string * string) list -> name:string -> xml list -> xml


module type Atom = sig
  type t
  val of_string  : string -> t
  val to_string  : t -> string
  val xsd_format : Restriction.Format.t
  val xsd_restrictions : Restriction.t list
end

module Make (Atom : Atom) : Xmlable
  with type t := Atom.t

(** Helper functions to create the conversion functions by hand *)
val to_xml :
  to_string:('a -> string)
  -> 'a
  -> xml list


val of_xml :
  of_string:(string -> 'a)
  -> xml
  -> 'a

(** Creating an internal element in the xml tree *)
val create_node :
  tag:string
  -> body:xml list
  -> xml

(** Creating a leaf in the xml tree *)
val create_data : string -> xml

(** Conversion functions used by the camlp4 extension. Not to be used by hand *)
type 'a of_xml = xml -> 'a
val unit_of_xml      : unit of_xml
val bool_of_xml      : bool of_xml
val string_of_xml    : string of_xml
val char_of_xml      : char of_xml
val int_of_xml       : int of_xml
val float_of_xml     : float of_xml
val int32_of_xml     : Int32.t of_xml
val int64_of_xml     : Int64.t of_xml
val nativeint_of_xml : Nativeint.t of_xml
val big_int_of_xml   : Big_int.big_int of_xml
val nat_of_xml       : Nat.nat of_xml
val num_of_xml       : Num.num of_xml
val ratio_of_xml     : Ratio.ratio of_xml
val list_of_xml      : ?tag:string -> (xml -> 'a) -> 'a list of_xml
val array_of_xml     : tag:string -> (xml -> 'a) -> 'a array of_xml
val option_of_xml    : tag:string -> (xml -> 'a) -> 'a option of_xml
val ref_of_xml       : (xml -> 'a) -> 'a ref of_xml
val lazy_t_of_xml    : (xml -> 'a) -> 'a Lazy.t of_xml
val recursive_of_xml : string -> (xml -> 'a) -> 'a of_xml

type 'a to_xml = 'a -> xml list
val xml_of_unit      : unit to_xml
val xml_of_bool      : bool to_xml
val xml_of_string    : string to_xml
val xml_of_char      : char to_xml
val xml_of_int       : int to_xml
val xml_of_float     : float to_xml
val xml_of_int32     : Int32.t to_xml
val xml_of_int64     : Int64.t to_xml
val xml_of_nativeint : Nativeint.t to_xml
val xml_of_big_int   : Big_int.big_int to_xml
val xml_of_nat       : Nat.nat to_xml
val xml_of_num       : Num.num to_xml
val xml_of_ratio     : Ratio.ratio to_xml
val xml_of_ref       : ('a -> xml list) -> 'a ref to_xml
val xml_of_lazy_t    : ('a -> xml list) -> 'a Lazy.t to_xml
val xml_of_list      : tag:string -> ('a -> xml list) -> 'a list to_xml
val xml_of_array     : tag:string -> ('a -> xml list) -> 'a array to_xml
val xml_of_option    : tag:string -> ('a -> xml list) -> 'a option to_xml

type to_xsd = xml list
val xsd_of_unit      : to_xsd
val xsd_of_bool      : to_xsd
val xsd_of_string    : to_xsd
val xsd_of_char      : to_xsd
val xsd_of_int       : to_xsd
val xsd_of_float     : to_xsd
val xsd_of_int32     : to_xsd
val xsd_of_int64     : to_xsd
val xsd_of_nativeint : to_xsd
val xsd_of_big_int   : to_xsd
val xsd_of_list      : string -> to_xsd -> to_xsd
val xsd_of_array     : string -> to_xsd -> to_xsd
val xsd_of_nat       : to_xsd
val xsd_of_num       : to_xsd
val xsd_of_ratio     : to_xsd
val xsd_of_ref       : to_xsd -> to_xsd
val xsd_of_lazy_t    : to_xsd -> to_xsd
val xsd_of_option    : string -> to_xsd -> to_xsd

(** Converstion functions used for excaml... macs should be upgraded to use this
val list_xml : ('a -> xml list) -> 'a list to_xml *)

module type X = sig
  type t
  val add_string : t -> string -> unit
end

module Write(X:X) : sig
  val write : X.t -> xml -> unit
end
