(** Timer, Probe, Groups and Group points are all assigned globally unique [Id.t]s. *)

open! Core

(** Each probe ([Intf.S]) has a unique [Probe_id.t] assigned. *)
type t = private int [@@deriving bin_io, compare, sexp]
include Intable with type t := t
include Identifiable.S with type t := t

(** [create] returns a [Id.t] unique within this process *)
val create : unit -> t


