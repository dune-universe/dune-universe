(** File_tail is useful for pulling data from a file that is being appended to by another
    process.  Creating a file tail returns the reader half of a pipe whose writer half is
    populated by a background process that roughly does the following loop.

    {v
      loop:
        stat to find out if data is available
        read data (repeatedly [ open, seek, read, close ] until all data is read)
        wait for some time
    v} *)

open! Core
open! Import

module Error : sig
  (** Errors are written to the pipe, and are nonrecoverable.  After an error, the pipe
      will always be closed. *)
  type t =
    | File_replaced
    (** [File_replaced] occurs if the inode of the file changed and the file tail was
        configured with [ignore_inode_change = false]. *)
    | File_shrank
    (** [File_shrank] occurs if [stat] detects that the length of the file decreases from
        one call to the next. *)
    | Read_failed   of exn
    (** [Read_failed] occurs if some aspect of the open-seek-read-close used to get data
        fails. *)
    | Stat_failed   of exn
    (** [Stat_failed] occurs if [stat] fails. *)
  [@@deriving sexp_of]

  val to_string_hum : t -> string
end

module Warning : sig
  type t =
    | Did_not_reach_eof_for               of Time.Span.t
    (** [Did_not_reach_eof_for span] occurs if it has been longer than
        [eof_latency_tolerance] since [stat] detected that there is new data in the file
        and the file tail processed all the new data.  The [span] is how long it has been
        since [stat] detected new data in the file. *)
    | Reached_eof
    (** [Reached_eof] occurs whenever the file tail reaches the end of file, irrespective
        of whether there has previously been a [Did_not_reach_eof_for] warning. *)
    | Delayed_due_to_null_reads_for       of Time.Span.t
    (** [Delayed_due_to_null_reads_for span] occurs when the file tail is unable to get
        data from the file, because the data being read has null ('\000') characters.  The
        span is how long it has been attempting to read and been getting nulls.  This
        warning will only occur if [retry_null_reads = true].  This warning will be
        repeated until the null reads stop. *)
    | No_longer_delayed_due_to_null_reads
    (** [No_longer_delayed_due_to_null_reads] occurs after a nonempty sequence of
        [Delayed_due_to_null_reads_for] warnings, once the file tail gets a read that does
        not contain null reads. *)
  [@@deriving sexp_of]

  val to_string_hum : t -> string
end

module Update : sig
  type t =
    | Data    of string
    (** [Data string] contains a chunk of data from the file.  If [break_on_lines], then
        data will be a single line (without the terminating newline). *)
    | Warning of string * Warning.t
    (** Warnings do not close the stream and whatever is reading can keep on doing so.
        The [string] is the file name. *)
    | Error   of string * Error.t
    (** Errors cause the stream to be closed.  The [string] is the file name. *)
  [@@deriving sexp_of]

  val to_string_hum : t -> string
end

(** [create file] creates a [File_tail.t] that will immediately begin reading [file], and
    then will start the stat-read loop.

    [read_buf_len] sets the size of the internal buffer used for making read system calls.

    [read_delay] sets how long the stat-read loop waits each time after it reaches eof
    before stat'ing again.  Setting [read_delay] too low could cause unecessary load.

    If [retry_null_reads = true], then reads that return data with null ('\000')
    characters are ignored and cause the system to delay 0.2s and attempt the read again.
    If [retry_null_reads = false], then the file tail will process data with nulls just as
    it would any other data.

    If [break_on_lines = true], the file tail will break data into lines on '\n'.  If not,
    the fill tail will return chunks of data from the end of the file as they are
    available.

    If [ignore_inode_change = true], the file tail will silently press on when the
    [file]'s inode changes.  If not, an inode change will cause the file tail to report an
    error and stop.  CIFS changes inodes of mounted files few times a day and we need
    [ignore_inode_change = true] option to keep tailers watching files on it alive.

    [start_at] determines the file position at which the file tail starts.

    [eof_latency_tolerance] affects the [Did_not_reach_eof_for] warning.

    [null_read_tolerance] determines how long the tailing must observe null reads
    before it will report a [Delayed_due_to_null_reads_for] warning.

    The default [throttle] is a global throttle shared among all file tails that has
    [continue_on_error = true] and [max_concurrent_jobs = 50]. *)
val create
  :  ?read_buf_len          : int                    (** default is 32k *)
  -> ?read_delay            : Time.Span.t            (** default is 0.5s *)
  -> ?retry_null_reads      : bool                   (** default is [true] *)
  -> ?break_on_lines        : bool                   (** default is [true] *)
  -> ?ignore_inode_change   : bool                   (** default is [false] *)
  -> ?start_at              : [ `Beginning           (** default is [`Beginning] *)
                              | `End
                              | `Pos of Int64.t
                              ]
  -> ?eof_latency_tolerance : Time.Span.t            (** default is 5s *)
  -> ?null_read_tolerance   : Time.Span.t            (** default is 0s *)
  -> ?throttle              : unit Throttle.t
  -> string
  -> Update.t Pipe.Reader.t
