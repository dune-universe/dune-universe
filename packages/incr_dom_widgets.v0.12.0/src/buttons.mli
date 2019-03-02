open! Core_kernel
open Incr_dom.Vdom


type button_creator =
  ?extra_classes:Attr.t list
  -> ?on_click:(Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> Event.t)
  -> ?outlined:bool
  -> ?compact:bool
  -> ?title:string
  -> text:string
  -> unit
  -> Node.t

val default : button_creator
val primary : button_creator
val secondary : button_creator
val disabled : button_creator
