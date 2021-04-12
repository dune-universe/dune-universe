type t =
  | Nil
  | Integer of int64
  | Simple_string of string
  | Error of string
  | Bulk of [ `String of string | `Bytes of bytes ]
  | Array of t Seq.t

type lexeme =
  [ `Nil
  | `Integer of int64
  | `Simple_string of string
  | `Error of string
  | `Bs of int
  | `As of int ]

type error =
  [ `Msg of string | `Unexpected of char | `Invalid_value | `Invalid_encoder ]

module type Resp = sig
  type t =
    | Nil
    | Integer of int64
    | Simple_string of string
    | Error of string
    | Bulk of [ `String of string | `Bytes of bytes ]
    | Array of t Seq.t

  type lexeme =
    [ `Nil
    | `Integer of int64
    | `Simple_string of string
    | `Error of string
    | `Bs of int
    | `As of int ]

  type error =
    [ `Msg of string | `Unexpected of char | `Invalid_value | `Invalid_encoder ]

  val pp_error : Format.formatter -> error -> unit

  val string_of_error : error -> string

  val unwrap : ('a, error) result -> 'a

  exception Exc of error

  module type INPUT = sig
    type ic

    val read : ic -> int -> string Lwt.t

    val read_line : ic -> string Lwt.t

    val read_char : ic -> char Lwt.t
  end

  module type OUTPUT = sig
    type oc

    val write : oc -> string -> unit Lwt.t
  end

  module type READER = sig
    include INPUT

    val read_lexeme : ic -> (lexeme, error) result Lwt.t

    val decode : ic -> lexeme -> t Lwt.t
  end

  module type WRITER = sig
    include OUTPUT

    val write_sep : oc -> unit Lwt.t

    val write_lexeme : oc -> lexeme -> unit Lwt.t

    val encode : oc -> t -> unit Lwt.t
  end

  module type S = sig
    module Reader : READER

    module Writer : WRITER

    val write : Writer.oc -> t -> unit Lwt.t

    val read : Reader.ic -> t Lwt.t
  end

  module Reader (I : INPUT) : READER with type ic = I.ic

  module Writer (O : OUTPUT) : WRITER with type oc = O.oc

  module Make (Reader : READER) (Writer : WRITER) :
    S with module Reader = Reader and module Writer = Writer

  module String_reader : READER with type ic = string ref

  module String_writer : WRITER with type oc = string ref

  module String :
    S with module Reader = String_reader and module Writer = String_writer

  val is_nil : t -> bool

  val to_string : t -> (string, error) result

  val to_string_exn : t -> string

  val to_bytes : t -> (bytes, error) result

  val to_bytes_exn : t -> bytes

  val to_integer : t -> (int64, error) result

  val to_integer_exn : t -> int64

  val to_float : t -> (float, error) result

  val to_float_exn : t -> float

  val to_array : (t -> 'b) -> t -> ('b array, error) result

  val to_array_exn : (t -> 'b) -> t -> 'b array

  val to_list : (t -> 'b) -> t -> ('b list, error) result

  val to_list_exn : (t -> 'b) -> t -> 'b list

  val to_seq : (t -> 'b) -> t -> ('b Seq.t, error) result

  val to_seq_exn : (t -> 'b) -> t -> 'b Seq.t

  val to_alist : (t -> 'k) -> (t -> 'v) -> t -> (('k * 'v) list, error) result

  val to_alist_exn : (t -> 'k) -> (t -> 'v) -> t -> ('k * 'v) list

  val to_hashtbl :
    (t -> 'k) -> (t -> 'v) -> t -> (('k, 'v) Hashtbl.t, error) result

  val to_hashtbl_exn : (t -> 'k) -> (t -> 'v) -> t -> ('k, 'v) Hashtbl.t

  val int : int -> t

  val int64 : int64 -> t

  val float : float -> t

  val bytes : bytes -> t

  val string : string -> t

  val simple_string : string -> t

  val nil : t

  val array : ('a -> t) -> 'a array -> t

  val list : ('a -> t) -> 'a list -> t

  val alist : ('k -> t) -> ('v -> t) -> ('k * 'v) list -> t

  val hashtbl : ('k -> t) -> ('v -> t) -> ('k, 'v) Hashtbl.t -> t

  val id : 'a -> 'a

  val equal : t -> t -> bool
end
