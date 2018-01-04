open! Core
open! Async
open! Shared

(** All readers defined below will raise if they encounter unparsable content. *)

module type Deprecated_csv = sig

  module Row : Row_intf.Row

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
