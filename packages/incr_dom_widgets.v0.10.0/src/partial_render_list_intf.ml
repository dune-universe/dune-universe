open! Core_kernel
open! Import
open Util

module Interval = struct
  type 'a t =
    | Empty
    | Range of 'a * 'a
  [@@deriving compare, sexp]
end

module Measurements = struct
  type t = { list_rect: float Js_misc.Rect.t
           ; view_rect: float Js_misc.Rect.t }
  [@@deriving compare, sexp]
end

module type Key = sig
  type t [@@deriving sexp, compare]
  include Comparable.S with type t := t
end

(** [Partial_render_list] provides common functionality for partially rendering large (e.g
    10_000 rows) lists or tables. It allows apps to measure and cache the height of each
    row and incrementally compute which rows to show while scrolling. It puts spacers
    above and below the viewport so that the list element remains the same height with
    only as many rendered rows as necessary to fill the viewport. This approach allows
    high data change rates because it doesn't change the dom for rows that are not in
    view.

    Because of the height cache your rows don't all have to be the same height. It is fine
    if some rows have more data or the editing UI is taller than a display row.

    See lib/incr_dom/examples/ts_gui for a demonstration of how to use of this module.
*)
module type S = sig
  module Key : Key

  (** Height_cache keeps track of the rendered height of items so that scrolling
      to a given position can decide which elements to render accurately. This allows
      rows to have different heights and change height at runtime.

      It only caches heights for rows that are currently in the list with a given key, so
      items will be dropped on changing a filter or sort, but this is only noticable if
      the height guess is wrong and the user is paying very close attention to consistency
      of scroll positions.

      The [height_guess] parameter is the default height in pixels returned for any item
      that is not in the cache. Rows that are measured to be exactly [height_guess] tall
      will not even be added to the cache. If most of your rows are a certain size you
      should determine the exact height returned to [measure_heights] in the typical case
      and use that as your guess. *)
  module Height_cache : sig
    type t [@@deriving compare, sexp_of]

    val empty : height_guess:float -> t

    (** [height t key] will return the actual height of [key] if available, otherwise it
        returns [height_guess] *)
    val height : t -> Key.t -> float
  end

  (** Meant to be stored in the derived model *)
  type 'v t

  val create
    :  rows:'v Key.Map.t Incr.t
    -> height_cache:Height_cache.t Incr.t
    -> measurements:Measurements.t option Incr.t
    -> 'v t Incr.t

  val find_by_position : _ t -> position:float -> Key.t option

  (* [find_by_relative_position t key ~offset] returns the key at a distance of
     approximately [offset] away from [key], preferring closer elements to farther ones.
     If the offset extends past the end of the list, the end key is returned instead. *)
  val find_by_relative_position : _ t -> Key.t -> offset:float -> Key.t option

  (** Meant for rendering, apps should normally use Incr.Map.mapi' on this *)
  val rows_to_render : 'v t -> 'v Key.Map.t

  (** (top, bottom) spacer pixel heights to put the rendered rows in the right place *)
  val spacer_heights : _ t Incr.t -> (float * float) Incr.t

  (** Scroll the view to the row with the given key, scrolling the minimum amount possible
      while still being [(top|bottom)_margin] pixels away from the top and bottom of the
      screen. If this can't be satisfied, the margin will be reduced to the point where it
      can. If a margin of 0 can't show the whole element, it will prefer showing the top.

      [?in] determines the element which should be scrolled. Default is `Window *)
  val scroll_into_scroll_region
    : ?in_           : Scroll_region.t
    -> _ t
    -> top_margin    : float
    -> bottom_margin : float
    -> key           : Key.t
    -> Scroll_result.t

  val scroll_to_position
    :  ?in_     : Scroll_region.t
    -> _ t
    -> position : float
    -> key      : Key.t
    -> Scroll_result.t

  val scroll_to_position_and_into_region
    : ?in_ : Scroll_region.t
    -> _ t
    -> position      : float
    -> top_margin    : float
    -> bottom_margin : float
    -> key           : Key.t
    -> Scroll_result.t

  (** [is_in_region] and [get_position] return [None] if the given key does not exist or
      the view rect and list rect measurements are not yet available. *)

  val is_in_region
    :  _ t
    -> top_margin    : float
    -> bottom_margin : float
    -> key           : Key.t
    -> bool option

  val get_position
    :  _ t
    -> key : Key.t
    -> float option

  val get_top_and_bottom
    :  _ t
    -> key : Key.t
    -> (float * float) option

  (** [measure_heights_simple] updates a height cache by measuring the rendered elements,
      relying on the app to provide a function for finding and measuring the element for a
      given key.
      This function can be used for table with collapsed borders and box sizing set to
      border-box.
  *)
  val measure_heights_simple
    :  _ t
    -> measure:(Key.t -> float option)
    -> Height_cache.t

  (** [measure_heights] is like [measure_heights_simple], but allows the app to use the
      previous and next rows in addition to the current row to measure the current row's
      height (e.g. using the bottom position of the previous row and/or the top position
      of the next row).
      To avoid retaking the same measures three times for each row (once as the "previous"
      row, once as the "current" row, and once as the "next" row), [measure_row] allows
      the app to specify what measurements to take for a given row, and it is called
      exactly once per row.
      This function should be used for tables with non-collapsed borders.
  *)
  val measure_heights
    :  _ t
    -> measure_row:(Key.t -> 'm option)
    -> get_row_height:(prev:'m option -> curr:'m option -> next:'m option -> float option)
    -> Height_cache.t
end

module type Partial_render_list = sig
  module type S = S
  module type Key = Key
  module Interval = Interval
  module Measurements = Measurements
  module Make (Key : Key) : (S with module Key = Key)
end
