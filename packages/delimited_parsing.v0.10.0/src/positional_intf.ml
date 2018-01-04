open! Core
open! Async
open Shared

(** All readers defined below will raise if they encounter unparsable content. *)

module type Positional = sig

  module Row : Row_intf.Row

  (** Specify the name, and 0-based starting position and length of each column.
      For example column 'foo' starting on the first character of each line and 8
      characters wide would be ("foo", 0, 8).
      Column ranges must not overlap. *)
  type header = (string * int * int) list

  (** All following funtions return Error if column ranges overlap. *)

  (** [of_reader ?quote ?strip ?skip_lines ~sep ~header r] returns a row pipe based on data
      read from the provided reader.  [sep] is used as the separator between fields, and is
      assumed to be escaped with \ unless [quote] is given.  *)
  val of_reader :
    (header, ?strict:bool -> Reader.t -> Row.t Pipe.Reader.t Or_error.t) reader
  val create_reader :
    (header, ?strict:bool -> string -> Row.t Pipe.Reader.t Deferred.Or_error.t) reader

  val of_writer :
    Writer.t -> ?strict:bool -> header -> string list Pipe.Writer.t Or_error.t
  val create_writer :
    string -> ?strict:bool -> header -> string list Pipe.Writer.t Deferred.Or_error.t

end
