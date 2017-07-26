open! Core

open Core_profiler.Std_offline
open Core_profiler


module Header : sig
  (** A [Header.Item.t] is anything that is uniquely identified by a [Probe_id.t]. *)
  module Item : sig
    type single =
      { name : string
      ; spec : Probe_type.t
      }
    type group =
      { name : string
      ; points_spec : Probe_type.t
      ; children : Probe_id.t list
      }
    type group_point =
      { name : string
      ; parent : Probe_id.t
      ; sources : Probe_id.t list
      }

    type t =
      | Single of single
      | Group of group
      | Group_point of group_point

    val name : t -> string
  end

  type t = (Item.t, read) Id_table.t

  val find_exn             : t -> Probe_id.t -> Item.t
  val find_single_exn      : t -> Probe_id.t -> Item.single
  val find_group_exn       : t -> Probe_id.t -> Item.group
  val find_group_point_exn : t -> Probe_id.t -> Item.group_point

  (** Get a group point's parent *)
  val get_parent_id_exn : t -> Probe_id.t -> Probe_id.t
  val get_parent_exn    : t -> Probe_id.t -> Item.group

  (** If [add_group] is specified and the id refers to a group point,
      [group_name ^ add_group ^ group_point_name] is returned *)
  val get_name_exn : t -> ?with_group:string -> Probe_id.t -> string
  val get_spec_exn : t -> Probe_id.t -> Probe_type.t
  val get_units_exn : t -> Probe_id.t -> Profiler_units.t

  (** Conditions are ANDed, and default to true *)
  val create_table :
    t
    -> ?singles:bool
    -> ?groups:bool
    -> ?group_points:bool
    -> ?timers:bool
    -> ?probes:bool
    -> 'a
    -> ('a, _) Id_table.t
end

val consume_header : ([> read ], Iobuf.seek) Iobuf.t -> Profiler_epoch.t * Header.t

module Short_message : sig
  module Header : module type of Core_profiler.Protocol.Short_header

  type t =
    | Timer of Probe_id.t * Time_ns.t
    | Probe of Probe_id.t * Time_ns.t * int
    | Group_reset of Probe_id.t * Time_ns.t

  val id : t -> Probe_id.t
  val time : t -> Time_ns.t
end

val consume_short_message :
  ([> read ], Iobuf.seek) Iobuf.t
  -> Profiler_epoch.t
  -> Header.t
  -> Short_message.t

val fold_short_messages :
  ([> read ], _) Iobuf.t
  -> Profiler_epoch.t
  -> Header.t
  -> init:'accum
  -> f:('accum -> Short_message.t -> 'accum)
  -> 'accum

val iter_short_messages :
  ([> read ], _) Iobuf.t
  -> Profiler_epoch.t
  -> Header.t
  -> f:(Short_message.t -> unit)
  -> unit

val iteri_short_messages :
  ([> read ], _) Iobuf.t
  -> Profiler_epoch.t
  -> Header.t
  -> f:(int -> Short_message.t -> unit)
  -> unit

val map_file : string -> (read, _) Iobuf.t
