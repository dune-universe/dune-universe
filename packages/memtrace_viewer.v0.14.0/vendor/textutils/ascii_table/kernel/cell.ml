open! Core_kernel
open! Import
include Cell_intf

type t = Attr.t list * Text.t list [@@deriving sexp_of]

let attr = fst
let to_tuple = Fn.id
let create attr str = attr, List.map (String.split_lines str) ~f:Text.of_string
let width (_, lines) = list_max ~f:Text.width lines

let height (_, lines) ~display_empty_rows ~width =
  let height =
    list_sum lines ~f:(fun s -> max ((Text.width s + (width - 1)) / max width 1) 1)
  in
  if display_empty_rows then max height 1 else height
;;

let wrap (_, lines) ~width = List.bind lines ~f:(Text.chunks_of ~width)
let is_empty (_, lines) = List.for_all lines ~f:Text.is_empty
