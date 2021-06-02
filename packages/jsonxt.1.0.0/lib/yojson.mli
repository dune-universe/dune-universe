(** Yojson compatibility module

    To use Jsonxt's Yojson compatibility module create a [yojson.ml] file in
    the projects source directory with the following contents:
    {[
    include Jsonxt.Yojson
    ]}
    Note that compatibility is mostly a thin layer on top of Jsonxt.
    In particular the error reporting by the utils module uses
    the [Failure] exception rather than Yojson's specialist exceptions

    {2 JSON compatibility differences}

    The underlying parser used by the Yojson compatibility modules
    are RFC 8259 compliant except for specific extensions. In
    particular:
        - Control characters must be escaped as defined by RFC 8259
        - Comment are supported with /* */ and // at the end of the line
        - Variants and tuples are supported but with syntax changes noted
          later
        - Object keys must be quoted. So \{ab:10\} is not valid and must be
          encoded as \{"ab":10\}
        - Variant names must be quoted. eg. <"ab"> and <"ab":10>

    {2 Behavioural differences}
        - The [lexer_state] data structure is used to report errors but
          not updated during the parsing of the input
        - The optional [buf] parameter is ignored
        - Error messages are likely to be different
        - The utils module module uses the [Failure] exception rather
          than Yojson's specialist exceptions
 *)

exception Json_error of string

type lexer_state = {
  buf : Buffer.t;
  mutable lnum : int;
  mutable bol : int;
  mutable fname : string option;
}

val init_lexer : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> unit -> lexer_state

module Basic : sig
  type json = Json.Basic.json
  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Readers *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_channel : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_lexbuf : lexer_state -> ?stream:bool -> Lexing.lexbuf -> t
  val stream_from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_channel : ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> t Stream.t
  val stream_from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_lexbuf : lexer_state -> ?fin:(unit -> unit) -> Lexing.lexbuf -> t Stream.t

  val linestream_from_channel :
    ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> json_line Stream.t

  val linestream_from_file :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> json_line Stream.t

  val read_t : lexer_state -> Lexing.lexbuf -> t

  (* Writers *)

  val to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t -> string
  val to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t -> unit
  val to_file : ?len:int -> ?std:bool -> string -> t -> unit
  val to_outbuf : ?std:bool -> Buffer.t -> t -> unit
  val to_output : ?buf:Buffer.t -> ?len:int -> ?std:bool -> < output : string -> int -> int -> 'a; .. > -> t -> 'a
  val stream_to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t Stream.t -> string
  val stream_to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t Stream.t -> unit
  val stream_to_file : ?len:int -> ?std:bool -> string -> t Stream.t -> unit
  val stream_to_outbuf : ?std:bool -> Buffer.t -> t Stream.t -> unit
  val write_t : Buffer.t -> t -> unit

  (* Pretty printers *)
  val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  val pretty_to_string : ?std:bool -> t -> string
  val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  val prettify : ?std:bool -> string -> string
  val compact : ?std:bool -> string -> string

  (* Tools *)
  val equal : t -> t -> bool
  val sort : t -> t
  val show : t -> string
  val pp : Format.formatter -> t -> unit

  module Util : sig
    include (Process_intf.Shared with type json := json)
    include (Process_intf.Basic with type json := json)
  end
end

module Safe : sig
  type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]

  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Readers *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_channel : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_lexbuf : lexer_state -> ?stream:bool -> Lexing.lexbuf -> t
  val stream_from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_channel : ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> t Stream.t
  val stream_from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_lexbuf : lexer_state -> ?fin:(unit -> unit) -> Lexing.lexbuf -> t Stream.t

  val linestream_from_channel :
    ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> json_line Stream.t

  val linestream_from_file :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> json_line Stream.t

  val read_t : lexer_state -> Lexing.lexbuf -> t

  (* Writers *)

  val to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t -> string
  val to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t -> unit
  val to_file : ?len:int -> ?std:bool -> string -> t -> unit
  val to_outbuf : ?std:bool -> Buffer.t -> t -> unit
  val to_output : ?buf:Buffer.t -> ?len:int -> ?std:bool -> < output : string -> int -> int -> 'a; .. > -> t -> 'a
  val stream_to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t Stream.t -> string
  val stream_to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t Stream.t -> unit
  val stream_to_file : ?len:int -> ?std:bool -> string -> t Stream.t -> unit
  val stream_to_outbuf : ?std:bool -> Buffer.t -> t Stream.t -> unit
  val write_t : Buffer.t -> t -> unit

  (* Pretty printers *)
  val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  val pretty_to_string : ?std:bool -> t -> string
  val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  val prettify : ?std:bool -> string -> string
  val compact : ?std:bool -> string -> string

  (* Tools *)
  val sort : t -> t
  val equal : t -> t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val to_basic : t -> Basic.json

  module Util : sig
    include (Process_intf.Shared with type json := json)
    include (Process_intf.Basic with type json := json)
    include (Process_intf.Extended with type json := json)
  end
end

module Raw : sig
  type json =
    [
    | `Null
    | `Bool of bool
    | `Intlit of string
    | `Floatlit of string
    | `Stringlit of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]

  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Readers *)

  val from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_channel : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> in_channel -> t
  val from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t
  val from_lexbuf : lexer_state -> ?stream:bool -> Lexing.lexbuf -> t
  val stream_from_string : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_channel : ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> t Stream.t
  val stream_from_file : ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> t Stream.t
  val stream_from_lexbuf : lexer_state -> ?fin:(unit -> unit) -> Lexing.lexbuf -> t Stream.t

  val linestream_from_channel :
    ?buf:Buffer.t -> ?fin:(unit -> unit) -> ?fname:string -> ?lnum:int -> in_channel -> json_line Stream.t

  val linestream_from_file :
    ?buf:Buffer.t -> ?fname:string -> ?lnum:int -> string -> json_line Stream.t

  val read_t : lexer_state -> Lexing.lexbuf -> t

  (* Writers *)

  val to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t -> string
  val to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t -> unit
  val to_file : ?len:int -> ?std:bool -> string -> t -> unit
  val to_outbuf : ?std:bool -> Buffer.t -> t -> unit
  val to_output : ?buf:Buffer.t -> ?len:int -> ?std:bool -> < output : string -> int -> int -> 'a; .. > -> t -> 'a
  val stream_to_string : ?buf:Buffer.t -> ?len:int -> ?std:bool -> t Stream.t -> string
  val stream_to_channel : ?buf:Buffer.t -> ?len:int -> ?std:bool -> out_channel -> t Stream.t -> unit
  val stream_to_file : ?len:int -> ?std:bool -> string -> t Stream.t -> unit
  val stream_to_outbuf : ?std:bool -> Buffer.t -> t Stream.t -> unit
  val write_t : Buffer.t -> t -> unit

  (* Pretty printers *)
  val pretty_print : ?std:bool -> Format.formatter -> t -> unit
  val pretty_to_string : ?std:bool -> t -> string
  val pretty_to_channel : ?std:bool -> out_channel -> t -> unit
  val prettify : ?std:bool -> string -> string
  val compact : ?std:bool -> string -> string

  (* Tools *)
  val sort : t -> t
  val equal : t -> t -> bool
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end
