open Async

(** @inline *)
include
module type of Delimited_kernel.Write
  with module By_row := Delimited_kernel.Write.By_row

(** Make a pipe writer for ['a]s from a writer. The ['a]s will be written out as CSVs.

    The writer will be closed when the pipe closes. *)
val of_writer
  :  ?sep:char
  -> ?line_breaks:[`Unix | `Windows] (** default is [`Windows] *)
  -> write_header:bool
  -> 'a t
  -> Writer.t
  -> 'a Pipe.Writer.t

(** Make a pipe writer for ['a]s from a filename. The ['a]s will be written out as CSVs.

    The writer will be closed when the pipe closes. *)
val create_writer
  :  ?sep:char
  -> ?line_breaks:[`Unix | `Windows] (** default is [`Windows] *)
  -> write_header:bool
  -> 'a t
  -> string
  -> 'a Pipe.Writer.t

module By_row : sig
  (** @inline *)
  include module type of Delimited_kernel.Write.By_row

  val of_writer
    :  ?sep:char
    -> ?line_breaks:[`Unix | `Windows] (** default is [`Windows] *)
    -> Writer.t
    -> string list Pipe.Writer.t

  val create_writer
    :  ?sep:char
    -> ?line_breaks:[`Unix | `Windows] (** default is [`Windows] *)
    -> string
    -> string list Pipe.Writer.t Deferred.t
end
