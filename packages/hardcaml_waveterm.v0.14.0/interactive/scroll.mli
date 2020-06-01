open! Import

module Adjustment : sig
  type t =
    { mutable range : int
    ; mutable offset : int
    ; mutable on_offset_change : int -> unit
    }
  [@@deriving sexp_of]

  val set_range : ?trigger_callback:bool -> t -> int -> unit
end

module Scroll_bar_mode : sig
  type t =
    | Fixed of int
    | Dynamic of int
  [@@deriving sexp_of]
end

module Mouse_mode : sig
  type t =
    | Middle
    | Ratio
    | Auto
  [@@deriving sexp_of]
end

module Scroll_bar_style : sig
  type t =
    | Filled
    | Outline
  [@@deriving sexp_of]
end

module Orientation : sig
  type t =
    | Horz
    | Vert
  [@@deriving sexp_of]
end

module Scrollable : sig
  type t =
    { adj : Adjustment.t
    ; mutable scroll_window_size : int
    ; mutable scroll_bar_mode : Scroll_bar_mode.t
    ; mutable min_scroll_bar_size : int option
    ; mutable max_scroll_bar_size : int option
    ; mutable scroll_bar_size : int
    ; mutable scroll_bar_offset : int
    ; mutable mouse_mode : Mouse_mode.t
    ; mutable page_size : int
    ; mutable document_size : int
    ; mutable on_scrollbar_change : unit -> unit
    }
  [@@deriving sexp_of]

  val set_range : ?trigger_callback:bool -> t -> int -> unit
  val set_offset : ?trigger_callback:bool -> t -> int -> unit
end

module Scrollbar : sig
  type t =
    { scrollable : Scrollable.t
    ; mutable bar_style : Scroll_bar_style.t
    ; incr_key : Notty.Unescape.key
    ; decr_key : Notty.Unescape.key
    ; mutable bounds : Draw.rect
    ; orientation : Orientation.t
    }
  [@@deriving sexp_of]

  val mouse_event : t -> Notty.Unescape.mouse -> bool
  val key_event : t -> Notty.Unescape.key -> bool
  val set_bounds : t -> Draw.rect -> unit
end

module HScrollbar : sig
  type t = Scrollbar.t [@@deriving sexp_of]

  val create : Draw.rect -> t
  val draw : ctx:Draw_notty.ctx -> style:Draw_notty.style -> t -> unit
end

module VScrollbar : sig
  type t = Scrollbar.t [@@deriving sexp_of]

  val create : Draw.rect -> t
  val draw : ctx:Draw_notty.ctx -> style:Draw_notty.style -> t -> unit
end
