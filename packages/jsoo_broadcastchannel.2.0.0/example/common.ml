(** Some boilerplate :'( *)

open Js_of_ocaml

let fail () = raise (Failure "unable to find data")
let unopt x = Js.Opt.get x fail

let get_by_id id =
  Dom_html.document ## getElementById (Js.string id)
  |> unopt

let get_input x = 
  get_by_id x 
  |> Dom_html.CoerceTo.input
  |> unopt

let write_in element data = 
  let x = Dom_html.document ## createTextNode(Js.string data) in 
  let span = Dom_html.createDiv Dom_html.document in 
  let _ = Dom.appendChild span x in
  let _ =  Dom.insertBefore element span (element##.firstChild) in ()

let puts x = Firebug.console##log(x)

let iter_children f node =
  let nodeL = node##.childNodes in
  let len = nodeL##.length in
  for i = 0 to (pred len) do
    Js.Opt.iter (nodeL ## item(i)) f
  done

let remove_children fnode =
  let rec iter node =
    match Js.Opt.to_option (node##.firstChild) with
    | None -> ()
    | Some child ->
      let _ = node ## removeChild(child) in iter node
in iter fnode

let text_in element data = 
  let x = Dom_html.document ## createTextNode(Js.string data) in 
  let _ = remove_children element in 
  let _ =  Dom.appendChild element x in ()

