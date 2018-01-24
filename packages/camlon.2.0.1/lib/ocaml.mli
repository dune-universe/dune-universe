(** The data type.

    Note on lists:

    * List literals [[..]], including the null list [[]], are parsed and prnited 
      as [List ..], not with [Variant (..)].
    * Cons cells [x::xs] are parsed and printed using [Variant (..)].

    Record style Variant arguments are expressed as Varaint (Record ..).
 *)
type t =
  | Bool of bool
  | Int31       of int (** int for 32 bit arch *)
  | Int63       of int64 (** int for 64 bit arch *)
  | Int32       of int32
  | Int64       of int64
  | Nativeint32 of int32 (** natint for 32 bit arch *)
  | Nativeint64 of int64 (** natint for 64 bit arch *)
  | Float  of float
  | Char   of char
  | String of string
  | List   of t list
  | Array  of t list
  | Variant      of string * t list (** Variants. *)
  | Poly_variant of string * t list (** Polymorphic variants *)
  | Record of (string * t) list
  | Object of (string * t) list (** Object *)
  | Tuple of t list
  | Unit
  | Escaped of string (** Something outside of OCaml values *)

type ocaml = t

val escape_string : string -> string
(** Escape the special characters of the given string like [String.escaped].
    
    [String.escaped] and the format string's ["%S"] is too mean for UTF8 strings.
    [escape_string] does not escape characters whose code is more than 128.

    This function returns the argument itself if there is no need of escaping.
 *)

val format : Format.formatter -> t -> unit
(** Formatter of type [t] *)

val format_no_poly : Format.formatter -> t -> unit
(** Formatter of type [t].  Same as [format], 
    but prints polymorphic variants and objects as variants and records *)

val format_with 
  : ('a -> t) (*+ encoder *)
    -> Format.formatter 
    -> 'a (*+ the value *)
    -> unit
(** Same as [format] but,
    but prints polymorphic variants and objects as variants and records *)

val format_no_poly_with 
  : ('a -> t) (*+ encoder *)
    -> Format.formatter 
    -> 'a (*+ the value *)
    -> unit
(** Same as [format_with] but  *)

module Parser : sig
  
  type error = [ `Invalid_construct of Location.t
               | `Lexer of Location.t * Lexer.error
               | `Parser of Syntaxerr.error
	       | `Syntax_error of Location.t
               | `Exn of exn ]

  exception Error of error

  val loc_of_error : error -> Location.t
  (** Returns the location of the error.  [Loaction.none] is returned when
      the location is unknown. *)
    
  val format_error : Format.formatter -> error -> unit

  (** Parsers. They are not re-entrant, since OCaml's lexer is not. *)
  val from_lexbuf   : ?source:string -> Lexing.lexbuf -> t list
  val from_channel  : ?source:string -> in_channel -> t list
  val from_string   : ?source:string -> string -> t list
  val from_function : ?source:string -> (bytes -> int -> int) -> t list
end

type 'a load_error = [ `Conv of 'a | Parser.error ]

val load 
    : string (*+ file path *)
      -> (t list, Parser.error) result
(** Load values of [t] printed in a file *)   

val load_with 
    : (t -> ('a, 'b) result) (*+ decoder *)
      -> string (*+ file path *)
      -> ('a list, 'b load_error) result
(** Load values of [t] printed in a file, then convert them using the decoder *)   

val save 
  : perm:int (*+ Permission, ex. 0o644 *)
    -> ?no_poly:bool (*+ If true, prints polymorphic variants and objects as variants and records *)
    -> string (*+ file path *)
    -> t list
    -> unit
(** Save values of [t] to a file *)

val save_with 
  : ('a -> t) (*+ Encoder *)
    -> perm:int (*+ Permission,  ex. 0o644 *)
    -> ?no_poly:bool (*+ If true, prints polymorphic variants and objects as variants and records *)
    -> string (*+ file path *)
    -> 'a list (*+ values *)
    -> unit
(** Save OCaml values to a file, converting them to type [t] *)
