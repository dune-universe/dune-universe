open! Core_kernel
open! Import

module Id : sig
  include Identifiable

  val create
    :  column_index : int
    -> cell_index : int
    -> t

  val column_index : t -> int
  val cell_index : t -> int
end

module Mode : sig
  type t =
    | View
    | Edit of string Id.Map.t
  [@@deriving compare]
end

module Kind : sig
  type 'a t

  val string         : string t
  val bool           : bool t
  val int            : ?delimiter:char -> unit -> int t
  val float          : ?decimals:int -> ?delimiter:char -> ?strip_zero:bool -> unit -> float t
  val dollar         : float t
  val percent        : Percent.t t
  val span           : Time_ns.Span.t t
  val compact_int    : int t
  val compact_float  : float t
  val compact_dollar : float t
  val compact_num
    :  ?prepend:string
    -> ?append:string
    -> of_string:(string -> 'a)
    -> to_float:('a -> Float.t)
    -> unit
    -> 'a t

  (** [none_string] encodes how [None] is displayed (default ""). *)
  val option : ?none_string:string -> 'a t -> 'a option t
  val or_error : 'a t -> 'a Or_error.t t

  val user_defined
    :  name:string
    -> of_string:(string -> 'a)
    -> to_string:('a -> string)
    -> 'a t
end

type ('row, 'a) t

module Packed : sig
  type ('row, 'a) cell = ('row, 'a) t
  type 'row t =
    | T : ('row, _) cell -> 'row t
end

val pack : ('row, _) t -> 'row Packed.t

val create
  :  'a Kind.t
  -> read   : ('row -> 'a)
  -> ?write : ('row -> 'a -> 'row)
  -> ?style : ('a -> Css.t)
  -> unit
  -> ('row, 'a) t

val read : ('row, 'a) t -> 'row -> 'a

val view
  :  'row Packed.t
  -> 'row
  -> Id.t
  -> Mode.t
  -> html_id:string
  -> remember_edit:(Id.t -> string -> Vdom.Event.t)
  -> Vdom.Node.t

val apply_edit
  :  'row Packed.t
  -> 'row
  -> string
  -> 'row Or_error.t

val is_editable : _ Packed.t -> bool

(* Convenience funtions *)

(** [status a i n d] returns a list of [t]s to display the status of a collection
    of systems. *)
val status
  :  num_active:('row -> int)
  -> num_inactive:('row -> int)
  -> num_not_synchronized:('row -> int)
  -> num_dead:('row -> int)
  -> 'row Packed.t list

module Style : sig
  (** This module provides standard styling for [Cell.t]s. Values can be passed directly to
      [Cell.create]'s [?style] arg. *)

  type 'a t = 'a -> Css.t

  val concat : 'a t list -> 'a t

  val bool : bool t

  type theme = [ `Light | `Dark ]

  (** [compact_num] provides the standard compact number coloring given a [to_float]
      function. [compact_float] and [compact_int] are provided as a convenience. *)
  val compact_num : theme:theme -> ('a -> float) -> 'a t
  val compact_float : theme:theme -> float t
  val compact_int : theme:theme -> int t
end
