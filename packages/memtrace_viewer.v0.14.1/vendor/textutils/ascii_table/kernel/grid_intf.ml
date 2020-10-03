(** A grid lays out cells with concrete dimensions. *)

open! Core_kernel
open! Import

module Display = struct
  type t =
    | Short_box
    | Tall_box
    | Line
    | Blank
    | Column_titles
  [@@deriving compare, sexp_of]
end

module type Grid = sig
  module Display = Display

  type t [@@deriving sexp_of]

  val create
    :  'a Column.t list
    -> 'a list
    -> display:Display.t
    -> display_empty_rows:bool
    -> header_attr:Attr.t list
    -> max_width:int
    -> spacing:int
    -> t

  val to_screen : t -> Screen.t
end

