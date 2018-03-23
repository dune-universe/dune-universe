open! Core
open! Async

(** An applicative interface for parsing values from a csv file. *)

module Header = Header

(** This provides an applicative interface for constructing values from a csv file.

    An ['a t] describes how to build an OCaml model ['a] for each row.

    See lib/async_extended/example/csv_example.ml for an example of usage.
*)
type 'a t

include Applicative.S with type 'a t := 'a t

module Let_syntax : sig
  module Let_syntax : sig
    include Applicative.S with type 'a t := 'a t
    module Open_on_rhs : sig
      val at_index : int -> f:(string -> 'a) -> 'a t
      val at_header : string -> f:(string -> 'a) -> 'a t
    end
  end
end

val at_index : int -> f:(string -> 'a) -> 'a t
val at_header : string -> f:(string -> 'a) -> 'a t

(** ['a on_invalid_row] specifies how to handle a row whose extents are known but whose
    contents cannot be converted to a value of type ['a].  The default is to raise.

    If a row's extents are unknown, the parser cannot continue and will always raise.
*)
type 'a on_invalid_row

(** [of_reader ?strip ?skip_lines ?sep ?quote ~init ~f r] produces a value by folding
    over a csv document read from [r].

    If [strip] is true, leading and trailing whitespace is stripped from each field.
    Default value is false.

    If [skip_lines] > 0, that many lines are skipped at the start of the input.
    Note that this skips lines without doing any CSV parsing of the lines being skipped,
    so newlines within a quoted field are treated identically to newlines outside a
    quoted field.
    Default value is 0.

    [sep] is the character that separates fields within a row.
    Default value is ','

    [quote] defines a character to use for quoting. The default is [ `Using '"' ] which
    implements the MS Excel convention: either a field is unquoted, or it has leading and
    trailing quotes and internal escaped characters are represented as quote-char char,
    e.g., {|"a|} for [a].  [ `No_quoting ] means all characters are literal.
 *)
val fold_reader :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** [of_reader' ?strip ?skip_lines ?sep ?quote ~init ~f r] works similarly to
    [of_reader], except for the [f] argument. [of_reader'] runs [f] on batches
    of [Row.t]s rather than running [f] on each individual row.
*)
val fold_reader' :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a Queue.t -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

val fold_reader_without_pushback :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> Reader.t
  -> 'b Deferred.t

val fold_reader_to_pipe :
  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> Reader.t
  -> 'a Pipe.Reader.t

val fold_string :
  ?strip:bool
  -> ?sep:char
  -> ?quote:[ `No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a on_invalid_row
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> string
  -> 'b

(** Low-level interface *)

module Fast_queue : sig
  type 'a t

  val create   : ?capacity : int -> unit -> 'a t
  val of_list  : 'a list -> 'a t
  val enqueue  : 'a t -> 'a -> unit
  val nth_exn  : 'a t -> int -> 'a
  val clear    : 'a t    -> unit
  val to_list  : 'a t    -> 'a list
  val to_array : 'a t    -> 'a array
  val length   : 'a t    -> int
end

module On_invalid_row : sig
  type 'a t = 'a on_invalid_row

  val raise : _ t  (** The default. *)
  val skip : _ t

  val create
    :  (int String.Map.t       (** Map from header to position. *)
        -> string Fast_queue.t (** Value at each position. *)
        -> exn                 (** Exception raised when trying to convert this row. *)
        -> [ `Skip | `Yield of 'a | `Raise of exn ])
    -> 'a t

end

module Parse_state : sig

  (** At the lowest level, we model csv parsing as a fold over string arrays, one array
      per row. It is up to you to interpret the header row. *)

  type 'a t

  (** At any moment, the result of folding over all complete rows seen so far. *)
  val acc : 'a t -> 'a

  (** Can be used to set or clear the current [acc] *)
  val set_acc : 'a t -> 'a -> 'a t

  val create
    :  ?strip : bool
    -> ?sep : char
    -> ?quote:[ `No_quoting | `Using of char]
    (** Indices of the fields used. E.g., [~fields_used:(Some [| 0; 3; |])] means every
        row will be presented to [f] as having two fields, the first and fourth fields of
        the csv. This is for performance; pass [None] to store all fields.*)
    -> fields_used : int array option
    -> init : 'a
    (** [f i init row] should take the previous accumulator [init] and the next complete
        row [row], and return the next accumulator.

        The index [i] is the zero-indexed position of the next unconsumed byte relative to
        the start of this chunk of input. *)
    -> f : (int -> 'a -> string Fast_queue.t -> 'a)
    -> unit
    -> 'a t

  (** [input t ?pos ?len s] parses the first [len] characters of [s], starting at position
      [pos].  [pos] defaults to [0] and [len] defaults to reading up to the end of [s]. *)
  val input        : 'a t -> ?pos : int -> ?len : int -> Bytes.t -> 'a t
  val input_string : 'a t -> ?pos : int -> ?len : int -> string  -> 'a t

  (** [finish t] forces an end-of-row. Raises if end-of-row is not permitted here (e.g.,
      within a quoted field). It is permitted to [input] after a [finish]. *)
  val finish : 'a t -> 'a t
end

(** Backwards-compatible interface *)

module Builder : sig
  type nonrec 'a t = 'a t

  val lambda : (int String.Map.t -> string Fast_queue.t -> 'a) -> 'a t
  val return : 'a -> 'a t
end

val create_parse_state
  :  ?strip : bool
  -> ?sep : char
  -> ?quote:[ `No_quoting | `Using of char]
  -> ?on_invalid_row:'a on_invalid_row
  -> header_map:int String.Map.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> 'b Parse_state.t

module Header_parse : sig
  (** Type [t] represents an incomplete header parse. Keep calling [input] on it until you
      get a map from header name to column number. *)
  type t

  val create
    :  ?strip : bool
    -> ?sep : char
    -> ?quote : [ `No_quoting | `Using of char ]
    -> ?header : Header.t
    -> _ Builder.t
    -> (t, int String.Map.t) Either.t

  (** [input t ~len s] reads the first [len] bytes from [s] and returns either [t] or
      [header_map, unused_input]. *)
  val input        : t -> len : int -> Bytes.t -> (t, int String.Map.t * string) Either.t
  val input_string : t -> len : int -> string  -> (t, int String.Map.t * string) Either.t

  val is_at_beginning_of_row : t -> bool
end

module Row : sig
  include Row_intf.Row

  val create_of_fq : int String.Map.t               -> string Fast_queue.t -> t
  val upgrade      : ?header_map : int String.Map.t -> Row.t               -> t

  val header_map   : t -> int String.Map.t

  val builder : t Builder.t
end

(* row up to the error, and the field with the error up to the point of failure *)
exception Bad_csv_formatting of string list * string
