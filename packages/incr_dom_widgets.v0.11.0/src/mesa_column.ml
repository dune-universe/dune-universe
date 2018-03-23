open! Core_kernel
open! Import

open Incr.Let_syntax

module Id = struct
  include Int
  let of_index = Fn.id
end

type 'row t =
  { header : Vdom.Node.t
  ; cells : 'row Mesa_cell.Packed.t Mesa_cell.Id.Map.t
  ; style : Css.t
  ; cell_layout : [ `Horizontal | `Vertical ]
  ; sort_by : ('row -> Table.Default_sort_spec.Sort_key.t) option
  ; group : string option
  } [@@deriving fields]

let quick_header ?(sep = Vdom.Node.create "br" [] []) s =
  Vdom.Node.div
    [ Css.concat
        [ Css.text_align `Center
        ; Css.margin ~top:(`Px 2) ~bottom:(`Px 2) ~left:(`Px 7) ~right:(`Px 7) ()
        ]
      |> Css.to_attr
    ]
    (List.map s ~f:Vdom.Node.text
     |> List.intersperse ~sep)

let create
      ~header ~column_id ~cells
      ?(style=Css.empty)
      ?sort_by
      ?group
      ?(cell_layout=`Vertical)
      ()
  =
  let cells =
    List.mapi cells ~f:(fun cell_index cell ->
      ( Mesa_cell.Id.create ~column_index:column_id ~cell_index, cell ))
    |> Mesa_cell.Id.Map.of_alist_exn
  in
  { header; cells; style; cell_layout; sort_by; group }

let view t row mode ~cell_html_id ~remember_edit =
  let cells =
    Map.mapi t.cells ~f:(fun ~key:id ~data:cell ->
      let html_id = cell_html_id id in
      let%map row = row
      and mode = mode
      in
      Mesa_cell.view cell row id mode ~html_id ~remember_edit)
    |> Incr.Map.flatten
  in
  let flex_container =
    match t.cell_layout with
    | `Vertical   -> Css.flex_container ~direction:`Column ()
    | `Horizontal -> Css.flex_container ~direction:`Row    ()
  in
  let rows =
    let flex = Css.(to_attr (flex_item ~grow:1. ~basis:(`Px 0) ())) in
    Incr.Map.mapi cells ~f:(fun ~key:_ ~data:node ->
      Vdom.Node.div
        [ flex ]
        [ node ])
    >>| Map.data
  in
  let style_attr =
    Css.concat [ t.style; flex_container ]
    |> Css.to_attr
  in
  let%map rows = rows in
  Row_node_spec.(
    { Cell.
      attrs = Attrs.create ()
    ; node =
        Vdom.Node.div
          [ style_attr ]
          rows
    })

let creator l =
  List.mapi l ~f:(fun i f -> (i, f ~column_id:i))
  |> Id.Map.of_alist_exn
