open WebStorage
open Js_of_ocaml

let doc = Dom_html.document
let io x = Firebug.console##log(x)
let puts x = io (Js.string x)
let alert x = Dom_html.window##alert(Js.string x)
let fail () = raise (Failure "unable to find data")
let u x = Js.Opt.get x fail
let qs parent selector = u (parent##querySelector(Js.string selector))
let app = qs doc "#app"
let prepend elt parent =
  let _ = Dom.insertBefore parent elt (parent##.firstChild) in
  elt 

let span txt = 
  let t = doc##createTextNode(Js.string txt) in 
  let s = Dom_html.createSpan doc in 
  let _ = Dom.appendChild s t in 
  s

let create_div k li = 
  let d = Dom_html.createDiv doc in 
  let _ = d##.classList##add(Js.string k) in
  let _  = List.iter (fun e -> Dom.appendChild d e ) li in 
  d

let create_clear _ = 
  let d = create_div "clear" [] in
  ignore (prepend d app)

let create_insert k v _ = 
  let d = create_div "insert" [span k; span v] in 
  ignore (prepend d app)

let create_remove k _ _ = 
  let d = create_div "remove" [span k] in 
  ignore (prepend d app)

let create_update k o v _ = 
  let d = create_div "update" [span k; span o; span v] in 
  ignore (prepend d app)



let _ = Local.on_change (fun ev _ -> puts (Local.dump_change_state ev) )
let _ = Local.on_clear create_clear
let _ = Local.on_insert create_insert
let _ = Local.on_remove create_remove
let _ = Local.on_update create_update
