open! Core
open! Async

(** All readers defined below will raise if they encounter unparsable content. *)

module type Header = sig
  type t = [
    (* file has no header line, rows have no access by header *)
    | `No
    (* process header line as it appears *)
    | `Yes
    (* process header line as it appears, assert that it has at least the provided
       headers. Only the provided headers will be available by name to the processed
       rows. *)
    | `Limit of string list
    (* throw away header line, use provided headers *)
    | `Replace of string list
    (* file has no header line, use provided headers *)
    | `Add of string list
    (* the supplied transform function will be passed the headers as they are in the file
       and should return the headers it would like to use. *)
    | `Transform of (string list -> string list) sexp_opaque
  ] [@@deriving sexp_of]
end

module type Row = sig
  type t

  (* [get_conv_exn t header [%here] conv] extract the cell with column
     [header] from [row] and convert it using [conv].  If there is an
     error the error is raised including [row] [header] [error] and
     the source code position ([%here]). *)
  val get_conv_exn : t -> string -> Source_code_position.t -> (string -> 'a) -> 'a

  (* [get_exn t header] return the column of the row corresponding to header *)
  val get_exn : t -> string -> string

  (* [get_conv_opt_exn] is like [get_conv_exn], but empty strings are converted to
     [None].  Missing headers raise exceptions. *)
  val get_conv_opt_exn : t -> string -> Source_code_position.t -> (string -> 'a) -> 'a option

  (* [get t header] same as get_exn, but returns an option when the header
     was not found *)
  val get : t -> string -> string option

  (* [get_opt_exn] is like [get_exn], but empty strings are converted to [None].  Missing
     headers raise exceptions. *)
  val get_opt_exn : t -> string -> string option

  (* [nth_exn t i] return the ith column of t (indexed from 0) *)
  val nth_exn : t -> int -> string

  (* [nth_conv_exn t i [%here] conv] extract the ith column of [t]
     and convert it using [conv].  If there is an
     error the error is raised including [row] [i] [error] and
     the source code position ([%here]). *)
  val nth_conv_exn : t -> int -> Source_code_position.t -> (string -> 'a) -> 'a

  (* [nth t i] same as nth_exn, but returns an option in the case where t does not have at
     least i - 1 columns *)
  val nth : t -> int -> string option

  (* [to_list t] return all columns in the order they appear in the file *)
  val to_list : t -> string list

  (* [to_array t] return all columns in the order they appear in the file *)
  val to_array : t -> string array

  (* [headers t] return the header mapping (header -> position) available for the table
     this row is from.
  *)
  val headers : t -> int String.Table.t

  (* [size t] return the size in bytes of the data *)
  val size : t -> int

  (* [is_empty t] return true if the row contains only empty strings *)
  val is_empty : t -> bool

  val to_string : t -> string

  val sexp_of_t : t -> Sexp.t

  val fold : t -> init:'acc -> f:('acc -> header:string -> data:string -> 'acc) -> 'acc

  val iter : t -> f:(header:string -> data:string -> unit) -> unit

  val create : int String.Table.t -> string Queue.t -> t

  val equal : t -> t -> bool

  val compare : t -> t -> int
end

module type S = sig
  module Header : Header

  module Row : Row

  (** If strip is true (default is false) then spaces will be stripped from the beginning
      and end of fields.

      If [skip_lines] is given then that number of lines will be read and discarded from the
      top of the file or Reader.t given.

      If [on_parse_error] is `Raise any lines that fail to parse will raise an exception.
      If `Handle is given the offending line will be passed to the function given, which may
      then indicate that processing should continue or finish.
  *)
  type ('a,'b) reader =
    ?strip:bool
    -> ?skip_lines:int
    -> ?on_parse_error:[`Raise
                       | `Handle of (string Queue.t -> exn -> [`Continue | `Finish])]
    -> header:'a
    -> 'b

  (** proper csv file parsing is different than general delimited file parsing and requires
      special handling code *)
  module Csv : sig
    (* row up to the error, and the field with the error up to the point of failure *)
    exception Bad_csv_formatting of string list * string

    (** [create_manual ?strip ~header r] returns a function that allows you to
        feed strings into it, receiving back the rows as they are finished.

        It is explicitly allowed to pass Eof, then more data.  This is useful
        when tailing a file or when joining multiple files together.
    *)
    val create_manual
      :  ?strip:bool
      -> ?sep:char
      -> header:Header.t
      -> unit
      -> ([`Data of string | `Eof] -> Row.t list) Staged.t

    (** [of_reader ?strip ~header r] returns a row pipe based on data read from
        the provided reader.
    *)
    val of_reader
      : ?strip:bool
      -> ?skip_lines:int
      -> ?sep:char
      -> header:Header.t
      -> Reader.t
      -> Row.t Pipe.Reader.t

    (** [create_reader ?strip ~header filename] same as of_reader, but creates the reader
        for you *)
    val create_reader
      : ?strip:bool
      -> ?skip_lines:int
      -> ?sep:char
      -> header:Header.t
      -> string
      -> Row.t Pipe.Reader.t Deferred.t

    val of_writer
      :  ?sep:char
      -> ?line_breaks:[`Unix|`Windows] (** default is [`Windows] *)
      -> Writer.t
      -> string list Pipe.Writer.t

    val create_writer
      :  ?sep:char
      -> ?line_breaks:[`Unix|`Windows] (** default is [`Windows] *)
      -> string
      -> string list Pipe.Writer.t Deferred.t

    val parse_string
      :  ?strip:bool
      -> ?sep:char
      -> header:Header.t
      -> string
      -> Row.t list
  end

  module Positional : sig
    (** Specify the name, and 0-based starting position and length of each column.
        For example column 'foo' starting on the first character of each line and 8
        characters wide would be ("foo", 0, 8).
        Column ranges must not overlap. *)
    type header = (string * int * int) list

    (** All following funtions return Error if column ranges overlap. *)
    val of_reader :
      (header, ?strict:bool -> Reader.t -> Row.t Pipe.Reader.t Or_error.t) reader
    val create_reader :
      (header, ?strict:bool -> string -> Row.t Pipe.Reader.t Deferred.Or_error.t) reader

    val of_writer :
      Writer.t -> ?strict:bool -> header -> string list Pipe.Writer.t Or_error.t
    val create_writer :
      string -> ?strict:bool -> header -> string list Pipe.Writer.t Deferred.Or_error.t
  end

  (** [of_reader ?quote ?strip ?skip_lines ~sep ~header r] returns a row pipe based on data
      read from the provided reader.  [sep] is used as the separator between fields, and is
      assumed to be escaped with \ unless [quote] is given.  *)
  val of_reader :
    (Header.t, ?quote:char -> sep:char -> Reader.t -> Row.t Pipe.Reader.t) reader

  (** [create_reader ?strip ?skip_lines ~header filename] same as of_reader, but creates
      the reader for you *)
  val create_reader :
    (Header.t, ?quote:char -> sep:char -> string -> Row.t Pipe.Reader.t Deferred.t) reader

end
