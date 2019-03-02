open! Core_kernel
open Incr_dom.Vdom

module Style = struct
  type t =
    | Default
    | Primary
    | Secondary
    | Disabled

  let to_attr t =
    match t with
    | Default -> None
    | Primary -> Some (Attr.class_ "primary")
    | Secondary -> Some (Attr.class_ "secondary")
    | Disabled -> Some Attr.disabled
  ;;
end

type button_creator =
  ?extra_classes:Attr.t list
  -> ?on_click:(Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> Event.t)
  -> ?outlined:bool
  -> ?compact:bool
  -> ?title:string
  -> text:string
  -> unit
  -> Node.t

let button
      ~style
      ?(extra_classes = [])
      ?on_click
      ?(outlined = false)
      ?(compact = false)
      ?title
      ~text
      ()
  =
  let on_click = Option.map on_click ~f:Attr.on_click |> Option.to_list in
  let title = Option.map title ~f:(Attr.create "title") |> Option.to_list in
  let outlined = if outlined then [ Attr.class_ "outlined" ] else [] in
  let compact = if compact then [ Attr.class_ "compact" ] else [] in
  let style = Style.to_attr style |> Option.to_list in
  Node.button
    (Attrs.merge_classes_and_styles
       ([ Attr.class_ "clickable" ]
        @ style
        @ on_click
        @ title
        @ outlined
        @ compact
        @ extra_classes))
    [ Node.text text ]
;;

let default = button ~style:Style.Default
let primary = button ~style:Style.Primary
let secondary = button ~style:Style.Secondary
let disabled = button ~style:Style.Disabled
