open! Core_kernel
open! Import
include Ascii_table_kernel_intf
module Align = Column.Align
module Attr = Attr
module Column = Column
module Table_char = Table_char

module Display = struct
  type t = Grid.Display.t =
    | Short_box
    | Tall_box
    | Line
    | Blank
    | Column_titles
  [@@deriving compare, sexp_of]

  let short_box = Short_box
  let tall_box = Tall_box
  let line = Line
  let blank = Blank
  let column_titles = Column_titles
end

module Screen = struct
  (* [Screen] is mostly private stuff, so we explicitly export the public bits instead of
     saying [Private] everywhere. *)

  type t = Screen.t

  let render = Screen.render
  let to_string = Screen.to_string
end

let draw
      ?(display = Display.short_box)
      ?(spacing = 1)
      ?(limit_width_to = 90)
      ?(header_attr = [])
      ?(display_empty_rows = false)
      cols
      data
  =
  match cols with
  | [] -> None
  | _ :: _ ->
    Some
      (Grid.create
         ~spacing
         ~display
         ~max_width:limit_width_to
         ~header_attr
         cols
         data
         ~display_empty_rows
       |> Grid.to_screen)
;;

module Private = struct
  module Text = Text
end
