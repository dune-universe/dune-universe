open Incr_dom.Vdom

val modal
  :  on_close:(Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t -> Event.t)
  -> modal_body:Node.t
  -> Node.t
