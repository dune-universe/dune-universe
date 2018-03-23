open! Core
open! Async
open Shared

module type Character_separated_without_quoting = sig

  module Row : Row_intf.Row

  (** All readers defined below will raise if they encounter unparsable content. *)


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
