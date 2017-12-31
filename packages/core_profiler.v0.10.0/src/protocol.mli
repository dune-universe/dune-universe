open! Core

val default_output_filename : string
val set_current_output_filename : string -> unit

(** Packs [Probe_id.t] and [Time_ns.t] into a single [int]. *)
module Short_header : sig
  (** The goal of [Short_header] is to pack a [Timer.record] into one word
      (we want to write an integer number of words for alignment, and don't
      want to pay for the size or time of writing a second word if we don't
      have to).

      A short header "contains" an [Probe_id.t] and a [Time_ns.t].
      A short header is a single word / integer; we have 63 bits to play with.
      The 9 most significant bits contain [Probe_id.to_int_exn]; the remaining 54 bits
      contain a time, stored as a number of nanoseconds from some [Profiler_epoch.t]
      (the epoch is written into the header; see [Writer.write_epoch]).

      2 ** 54 nanoseconds is approximately 208 days.
      The epoch is set to equal a little before now when OCaml starts up, so the header
      should continue to work for ~208 days after that.
  *)

  val id_bits : int
  val time_bits : int

  val max_id : int
  val max_time_diff : Time_ns.Span.t

  val pack_exn    : Profiler_epoch.t -> Probe_id.t -> Time_ns.t -> int
  val pack_unsafe : Profiler_epoch.t -> Probe_id.t -> Time_ns.t -> int
  val unpack_id   : int -> Probe_id.t
  val unpack_time : Profiler_epoch.t -> int -> Time_ns.t
  val unpack : Profiler_epoch.t -> int -> Probe_id.t * Time_ns.t
end

(** Handles allocating [Iobuf.t] and making sure there's enough space in it. *)
module Buffer : sig
  val get_header_chunk : unit -> (read, _) Iobuf.t
  val ensure_free : int -> unit

  (** All of these will push the current chunk into the list of
      previous chunks first; a new chunk will then be allocated on the next
      write. *)

  (** Is the main (short message) buffer empty? *)
  val is_empty : unit -> bool

  val get_chunks : unit -> (read_write, Iobuf.no_seek) Iobuf.t list

  (** To aid producing test cases for Reader. *)
  module Unsafe_internals : sig
    val reset : unit -> unit
  end
end

(** The [Writer] module contains functions that invoke parts of [Header_protocol] and
    [Short_header] in order to write into the relevant (global variable) buffers
    in [Buffer] *)
module Writer : sig
  val epoch : Profiler_epoch.t
  val max_time : Time_ns.t

  (** These write into the header chunk *)
  val write_new_single : Probe_id.t -> string -> Probe_type.t -> unit
  val write_new_group : Probe_id.t -> string -> Probe_type.t -> unit
  val write_new_group_point :
    group_id:Probe_id.t ->
    id:Probe_id.t ->
    string ->
    Probe_id.t array ->
    unit

  (** These write into the short message buffer *)
  val write_timer_at : Probe_id.t -> Time_ns.t -> unit
  val write_probe_at : Probe_id.t -> Time_ns.t -> int -> unit
  val write_group_reset : Probe_id.t -> Time_ns.t -> unit

  val dump_stats : unit -> unit

  (** Choose what to do with the in memory stats data at exit.
      The handler function is passed the header chunk and the list of data chunks.
      Defaults to [`Write_file "stats.dat"]. *)
  val set_at_exit_handler :
    [ `Write_file of string
    | `Function of
        ( (read, Iobuf.no_seek) Iobuf.t
          -> (read, Iobuf.no_seek) Iobuf.t list
          -> unit)
    | `Disable
    ]
    -> unit

  (** To aid producing test cases for Reader. *)
  module Unsafe_internals : sig
    val write_epoch : unit -> unit
    val write_end_of_header : unit -> unit
  end
end
