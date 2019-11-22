open Async

include module type of Delimited_kernel.Write (** @inline *)

(** Make a pipe writer for ['a]s from a writer. The ['a]s will be written out as CSVs.

    Once [with_writer]'s return Deferred becomes determined, it is guaranteed
    that the whole CSV has hit the OS buffer.

    The writer will NOT be closed when the pipe closes. *)
val with_writer
  :  ?sep:char
  -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
  -> write_header:bool
  -> 'a t
  -> Writer.t
  -> f:('a Pipe.Writer.t -> 'b Deferred.t)
  -> 'b Deferred.t

(** Make a pipe writer for ['a]s from a filename, using the given CSV
    converter. *)
val with_file
  :  ?sep:char
  -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
  -> write_header:bool
  -> 'a t
  -> string
  -> f:('a Pipe.Writer.t -> 'b Deferred.t)
  -> 'b Deferred.t

module By_row : sig
  include module type of Delimited_kernel.Write.By_row (** @inline *)

  (** Make a pipe writer for a list of strings from a writer. The string list
      will be formatted as CSV.

      Once [with_writer]'s return Deferred becomes determined, it is guaranteed
      that the whole CSV has hit the OS buffer.

      The writer will NOT be closed when the pipe closes. *)
  val with_writer
    :  ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> Writer.t
    -> f:(string list Pipe.Writer.t -> 'a Deferred.t)
    -> 'a Deferred.t

  (** Make a pipe writer for a list of strings from a filename. The string list
      will be formatted as CSV. *)
  val with_file
    :  ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> string
    -> f:(string list Pipe.Writer.t -> 'a Deferred.t)
    -> 'a Deferred.t
end

(** Here be dragons. You may wish to use these functions over the bracketed
    interface above, but you MUST wait on [Pipe.upstream_flushed] after
    closing the pipe.  *)
module Expert : sig
  include module type of Delimited_kernel.Write.Expert (** @inline *)

  (** Make a pipe writer for ['a]s from a writer. The ['a]s will be written out as CSVs.

      WARNING: you MUST wait on [Pipe.upstream_flushed] before doing anything with
      the resulting file
  *)
  val of_writer
    :  ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> write_header:bool
    -> 'a t
    -> Writer.t
    -> 'a Pipe.Writer.t

  (** Make a pipe writer for ['a]s from a writer. The ['a]s will be written out as CSVs.

      The writer will be closed when the pipe closes.

      WARNING: you MUST wait on [Pipe.upstream_flushed] before doing anything with
      the resulting file
  *)
  val of_writer_and_close
    :  ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> write_header:bool
    -> 'a t
    -> Writer.t
    -> 'a Pipe.Writer.t

  (** Make a pipe writer for ['a]s from a filename. The ['a]s will be written out as CSVs.

      The writer will be closed when the pipe closes.

      WARNING: you MUST wait on [Pipe.upstream_flushed] before doing anything with
      the resulting file
  *)
  val create_writer
    :  ?sep:char
    -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
    -> write_header:bool
    -> 'a t
    -> string
    -> 'a Pipe.Writer.t Deferred.t

  module By_row : sig
    (** Make a pipe writer for a list of strings from a writer. The string list
        will be formatted as CSV.

        WARNING: you MUST wait on [Pipe.upstream_flushed] before doing anything with
        the resulting file
    *)
    val of_writer
      :  ?sep:char
      -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
      -> Writer.t
      -> string list Pipe.Writer.t

    (** Make a pipe writer for a list of strings from a writer. The string list
        will be formatted as CSV.

        The writer will be closed when the pipe closes.

        WARNING: you MUST wait on [Pipe.upstream_flushed] before doing anything with
        the resulting file
    *)
    val of_writer_and_close
      :  ?sep:char
      -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
      -> Writer.t
      -> string list Pipe.Writer.t

    (** Make a pipe writer for a list of strings from a filename. The string list
        will be formatted as CSV.

        WARNING: you MUST wait on [Pipe.upstream_flushed] before doing anything with
        the resulting file
    *)
    val create_writer
      :  ?sep:char
      -> ?line_breaks:[ `Unix | `Windows ] (** default is [`Windows] *)
      -> string
      -> string list Pipe.Writer.t Deferred.t
  end
end
