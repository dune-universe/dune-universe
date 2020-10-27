
let debug f = Printf.ksprintf (fun s -> Firebug.console##log (Js.string s)) f
let error f = Printf.ksprintf (fun s -> Firebug.console##error (Js.string s); failwith s) f

let (!!) s = Js.string s

let (!$) (coerce,kind) s =
  let node =
    Js.Opt.get
      ( Dom_html.document##getElementById(!! s) )
      ( fun () -> error "node id %s not found" s) in
  Js.Opt.get (coerce node)
    ( fun () -> error "node id %s is not of type %s" s kind)

let input = Dom_html.CoerceTo.input, "input"
let textarea = Dom_html.CoerceTo.textarea, "textarea"
let div = Dom_html.CoerceTo.div, "div"
let button = Dom_html.CoerceTo.button, "button"

let input_optal = !$ textarea "input-optal"
let input_data = !$ textarea "input-data"
let input_ocaml = !$ textarea "input-ocaml"

(* let output_optal = !$ div "output-optal" *)
let output_optal = !$ textarea "output-optal"

let go_button = !$ button "run-optal"

let get_optal () = Js.to_string input_optal##value
let get_data () = Js.to_string input_data##value

let remove_all_childs n =
  List.iter
    (fun child -> Dom.removeChild n child)
    (Dom.list_of_nodeList n##childNodes)

let text_node t = Dom_html.document##createTextNode(t)

(* let set_output s = *)
(*   remove_all_childs output_optal; *)
(*   Dom.appendChild output_optal (text_node !!s) *)

let set_output s = output_optal##value <- !!s

let loaded = debug "page loaded"
