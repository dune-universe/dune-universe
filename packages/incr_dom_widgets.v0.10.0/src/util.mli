open! Core_kernel
open! Import
open Js_of_ocaml

module Focus_dir : sig
  type t = Prev | Next [@@deriving sexp, compare]
end

module Margin : sig
  type t =
    { top    : float
    ; left   : float
    ; bottom : float
    ; right  : float
    }
  [@@deriving fields, compare, sexp_of]

  val none : t

  val uniform : float -> t

  val create : ?top:float -> ?left:float -> ?bottom:float -> ?right:float -> unit -> t

  val adjust : ?top:float -> ?left:float -> ?bottom:float -> ?right:float -> t    -> t
end

module Scroll_region : sig
  type t = Window | Element of Dom_html.element Js.t

  module Id : sig
    type t = Window | Element of string [@@deriving compare, sexp]
  end

  val of_id : Id.t -> t option
end

module Float_type : sig
  type t = None | Edge | Px_from_edge of int [@@deriving compare, sexp]

  (** [compute_offset] computes the extra offset from the edge of the viewport created by
      having a floating element (header or first column).
      [get_float_elem_size] is a function that measures the relevant size of the floating
      element if applicable (height for floating header, width for floating first column).
  *)
  val compute_offset : t -> get_float_elem_size:(unit -> float option) -> float

  val is_floating : t -> bool

  val px_from_edge : t -> int option
end

module Scroll_result : sig
  type t = [ `Scrolled | `Didn't_scroll ]

  val combine : t -> t -> t

  val scrolled : t -> bool
end

module Scroll : sig
  module Dir : sig
    type t = Horizontal | Vertical
  end

  (** For functions in this module, "start" and "end" should be interpreted as follows:
      - in the vertical direction, "start" means "top" and "end" means "bottom"
      - in the horizontal direction, "start" means "left" and "end" means "right"

      For instance, for [scroll_to_position Horizontal], [scroll_region_start] is the left
      position of the scroll region and [elem_start] is the left position of the element.
  *)

  (** [scroll_into_region] scrolls the element with position from [elem_start] to
      [elem_end] into the scroll region as specified by [scroll_region_start] and
      [scroll_region_end], with additional margins of [start_margin] and [end_margin]
      respectively. *)
  val scroll_into_region
    :  ?in_                : Scroll_region.t
    -> Dir.t
    -> start_margin        : float
    -> end_margin          : float
    -> scroll_region_start : float
    -> scroll_region_end   : float
    -> elem_start          : float
    -> elem_end            : float
    -> Scroll_result.t

  (** [scroll_to_position] scrolls the element with position from [elem_start] to
      [elem_end] to the given position relative to the start of the scroll region as
      specified by [scroll_region_start].

      In cases where it is not possible to scroll the element to the specified position
      (e.g scrolling an element at the bottom of the page to the top of the viewport), the
      element is scrolled as close as possible to the given position.
  *)
  val scroll_to_position
    :  ?in_                : Scroll_region.t
    -> Dir.t
    -> position            : float
    -> scroll_region_start : float
    -> elem_start          : float
    -> Scroll_result.t

  (** [scroll_to_position_and_into_region] first calls [scroll_to_position], followed by
      [scroll_into_region] with the updated element position. *)
  val scroll_to_position_and_into_region
    :  ?in_                : Scroll_region.t
    -> Dir.t
    -> position            : float
    -> start_margin        : float
    -> end_margin          : float
    -> scroll_region_start : float
    -> scroll_region_end   : float
    -> elem_start          : float
    -> elem_end            : float
    -> Scroll_result.t

  (** [is_in_region] checks if the element with position from [elem_start] to [elem_end]
      is within the boundaries of the scroll region as specified by [scroll_region_start]
      and [scroll_region_end] plus a margin of [start_margin] or [end_margin]
      respectively. *)
  val is_in_region
    :  start_margin        : float
    -> end_margin          : float
    -> scroll_region_start : float
    -> scroll_region_end   : float
    -> elem_start          : float
    -> elem_end            : float
    -> bool

  (** [get_position] returns the position of the element with start position [elem_start]
      relative to the start of the scroll region as specified by [scroll_region_start] *)
  val get_position
    :  scroll_region_start : float
    -> elem_start          : float
    -> float
end

(** [move_focus] gives the next element starting from the given key in the given direction
    according to the order of elements in the map.
    If the given key is [None], [move_focus] returns the first element in the map for
    direction [Next], and the last element in the map for direction [Prev].
    If the given key is already the last element in the table for direction [Next], or the
    first element in the table for direction [Prev], then [move_focus] returns [None]. *)
val move_focus
  :  ('key, 'value, _) Map.t
  -> 'key option
  -> Focus_dir.t
  -> ('key * 'value) option
