open Core
open Async

(** @open *)
include module type of Delimited_kernel.Read

(** Async helpers for delimited parsing *)

(** [fold_reader ?strip ?skip_lines ?sep ?quote ~init ~f r] produces a value by folding
    over a csv document read from [r]. The reader will be closed on EOF.

    If [strip] is true, leading and trailing whitespace is stripped from each field.
    Default value is false.

    If [skip_lines] > 0, that many lines are skipped at the start of the input.
    Note that this skips lines without doing any CSV parsing of the lines being skipped,
    so newlines within a quoted field are treated identically to newlines outside a
    quoted field.
    Default value is 0.

    [sep] is the character that separates fields within a row.
    Default value is ','

    [quote] defines a character to use for quoting. [ `Using '"' ] implements
    the MS Excel convention: either a field is unquoted, or it has leading and
    trailing quotes and internal escaped characters are represented as
    quote-char char, e.g., {i "\n} to escape a newline. [`No_quoting] means all
    characters are literal. The default is [`Using '"']
*)
val fold_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** [fold_reader' ?strip ?skip_lines ?sep ?quote ~init ~f r] works similarly to
    [fold_reader], except for the [f] argument. [fold_reader'] runs [f] on batches
    of [Row.t]s rather than running [f] on each individual row.
*)
val fold_reader'
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a Queue.t -> 'b Deferred.t)
  -> Reader.t
  -> 'b Deferred.t

(** Same as [fold_reader] but the fold function does not exert pushback on the fold. *)
val fold_reader_without_pushback
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> init:'b
  -> f:('b -> 'a -> 'b)
  -> Reader.t
  -> 'b Deferred.t

(** [pipe_of_reader t reader] produces a pipe reader of parsed values. *)
val pipe_of_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> Reader.t
  -> 'a Pipe.Reader.t

(** [create_reader filename] opens a reader for the given filename & returns a pipe of its parsed values. *)
val create_reader
  :  ?strip:bool
  -> ?skip_lines:int
  -> ?sep:char
  -> ?quote:[`No_quoting | `Using of char]
  -> ?header:Header.t
  -> ?on_invalid_row:'a On_invalid_row.t
  -> 'a t
  -> string
  -> 'a Pipe.Reader.t Deferred.t
