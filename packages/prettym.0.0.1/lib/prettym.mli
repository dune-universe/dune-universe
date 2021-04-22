type ppf
(** Abstract data corresponding to the encoder state and all its machinery. *)

type 'a t = ppf -> 'a -> ppf
(** The type for formatters of values of type ['a]. *)

type ('ty, 'v) order

val keval_order : (ppf -> 'v) -> ppf -> ('ty, 'v) order -> 'ty

val break : indent:int -> len:int -> ('v, 'v) order
(** [break ~indent ~len] tries to add [len] spaces into the current line. If it
    fails - if we overflow the current line - we insert a [new_line] and fill
    the beginning of the new line with [indent] space characters. *)

val fws : ('v, 'v) order
(** [fws] is a specialization of {!break} with [indent = 1] and [spaces = 1].
    This is the most used token to express an opportunity to break the line and
    be conform to [RFC 822] and the {i folding-whitespace} token. *)

val spaces : int -> ('v, 'v) order
(** [spaces n] is a specialization of {!break} with [spaces = n]. *)

val cut : ('v, 'v) order
(** [cut] gives an opportunity to add a break line without indentation and
    spaces. *)

val const : 'a t -> 'a -> ('v, 'v) order
(** [const pp v] serialize [v] with [pp]. *)

val atom : 'a t -> ('a -> 'v, 'v) order
(** [atom pp] expects a value ['a] and serialize it with [pp]. *)

val a : ('a t -> 'a -> 'v, 'v) order
(** [a] expects a {i serializer} [pp] and a value to serialize it then. *)

val ( !! ) : 'a t -> ('a -> 'v, 'v) order
(** Alias of {!atom}. *)

val ( $ ) : 'a t -> 'a -> ('v, 'v) order
(** Alias of {!const}. *)

val new_line : ('v, 'v) order
(** [new_line] inserts a new line regardless how many bytes the current line
    has. *)

val tbox : int -> ('v, 'v) order
(** [tbox indent] creates a new box to indent any contents inside with [indent]
    spaces. *)

val bbox : ('v, 'v) order
(** [bbox] creates a new box to indent any contents at the current position
    of the serializer. *)

val box : ('v, 'v) order

val close : ('v, 'v) order
(** [close] closes any boxes. *)

val using : ('b -> 'a) -> 'a t -> 'b t
(** [using f pp] apply [f] on the value and use [pp] then to serialize the
    result. *)

val string : string t
(** [string] permits to serialize a [string]. *)

val bytes : Bytes.t t
(** [bytes] permits to serialize a [bytes]. *)

val bigstring : Bigstringaf.t t
(** [bigstring] permits to serialize a {i bigstring}. *)

val breakable : string t
(** [breakable] permits to serialize a {i breakable} [string]. If the given
    [string] is larger than the {i margin}, the serializer is able to break the
    [string] to fit under the margin instead of to wait an opportunity (such as
    {!cut} or {!fws}) to break the line. *)

val char : char t
(** [char] permits to serialize a [char]. *)

val list : sep:'x t * 'x -> 'v t -> 'v list t
(** [list ~sep pp] permits to serialize a list of values which can be
    serialized with [pp]. Between each values, we apply [sep]. *)

val option : 'a t -> 'a option t

type ('ty, 'v) fmt =
  | [] : ('v, 'v) fmt
  | ( :: ) : ('x, 'v) order * ('v, 'r) fmt -> ('x, 'r) fmt

val concat : ('a, 'b) fmt -> ('b, 'c) fmt -> ('a, 'c) fmt
val keval : (ppf -> 'v) -> ppf -> ('ty, 'v) fmt -> 'ty
val eval : ppf -> ('ty, ppf) fmt -> 'ty

module Buffer : sig
  type t = Bigstring of Bigstringaf.t | String of string | Bytes of bytes

  val weight : t -> int
  (** Weight of {!t}. *)

  val sub : t -> int -> int -> t
  (** [sub t off len] does a sub operation of {!t} of [len] bytes starting at
     [off]. *)
end

module IOVec : sig
  type t = { buffer : Buffer.t; off : int; len : int }
  (** Type of IOVec. *)

  val weight : t -> int
  (** Weight of {!t}. *)

  val length : t -> int
  (** Length (in bytes) of {!t}. *)

  val lengthv : t list -> int
  (** Length (in bytes) of a list of {!t}. *)

  val shift : t -> int -> t
  (** [shift t n] shifts [n] bytes on [t]. *)

  val split : t -> int -> t * t
  (** [split t off] splits [t] at [off] point. *)

  val merge : t -> t -> t option
  (** [merge a b] tries to merge [a] and [b] into a new {!t}. *)
end

val io_buffer_size : int

val create :
  ?margin:int -> ?new_line:string -> emitter:(IOVec.t list -> int) -> int -> ppf
(** [create ?margin ?new_line ~emitter weight] creates a serializer bounded to
    [weight] elements. The serializer is limiter to emit [margin] bytes per
    lines and break the line with [new_line] (default to ["\r\n"]).

    [emitter] is called when the serializer is full. It gives to user what it
    needs to transmit to the output and it asks how many bytes the user was
    able to transmit. *)

val is_empty : ppf -> bool
(** [is_empty ppf] returns [true] if the serializer is empty. Otherwise,
    it returns [false]. *)

val flush : ppf -> ppf
(** [flush ppf] enforces the serializer to call the internal [emitter] to
    transmit out what is serialized - even if [ppf] is not full. *)

val kflush : (ppf -> 'v) -> ppf -> 'v
(** [kflush] is {!flush} with a [k]ontinuation. *)

val to_string : ?margin:int -> ?new_line:string -> 'a t -> 'a -> string
(** [to_string ?margin ?new_line pp v] serializes a value [v] with the
    serializer [pp] into a [string]. *)

val to_stream :
  ?margin:int -> ?new_line:string -> 'a t -> 'a -> unit -> string option
(** [to_stream ?margin ?new_line pp v] serializes a value [v] with the
    serializer [pp] and it returns a function [unit -> string option] to
    transmit out the result {i chunk by chunk} of the serialization. *)
