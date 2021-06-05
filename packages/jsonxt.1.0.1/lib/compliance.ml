module type S = sig
  type json
  type json_stream

  val lex_string : string -> string
  val lex_number : Tokens.token -> Tokens.token
  val lex_integer : Tokens.token -> Tokens.token
  val lex_largeint : Tokens.token -> Tokens.token
  val lex_tuple : Tokens.token -> bool
  val lex_variant : Tokens.token -> bool

  val comment_check : unit -> (unit, string) result

  val number_to_string : float -> string

  val number : [`Float of float | `Infinity | `Neginfinity | `Nan | `Floatlit of string ] -> json
  val integer : int -> json
  val largeint : string -> json
  val null : json
  val string : string -> json
  val bool : bool -> json
  val assoc : (string * json) list -> json
  val list : json list -> json
  val tuple : json list -> json
  val variant : string -> json option -> json

  (* streaming functions *)

  module Stream : sig
    val number : [`Float of float | `Infinity | `Neginfinity | `Nan | `Floatlit of string  ] -> json_stream
    val integer : int -> json_stream
    val largeint : string -> json_stream
    val null : json_stream
    val string : string -> json_stream
    val bool : bool -> json_stream

    val array_start : unit -> json_stream
    val array_end : unit -> json_stream
    val object_start : unit -> json_stream
    val object_end : unit -> json_stream
    val tuple_start : unit -> json_stream
    val tuple_end : unit -> json_stream
    val variant_start : unit -> json_stream
    val variant_end : unit -> json_stream
    val name : string -> json_stream
  end
end
