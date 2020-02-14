open Js_of_ocaml
open Js_of_ocaml_lwt

let doc = Dom_html.document
let io x = Firebug.console##log(x)
let puts x = io (Js.string x)
let alert x = Dom_html.window##alert(Js.string x)
let fail () = raise (Failure "unable to find data")
let u x = Js.Opt.get x fail
let qs parent selector = u (parent##querySelector(Js.string selector))
let to_i elt = u (Dom_html.CoerceTo.input elt)

let clear_btn = qs doc "#clear"

let add_div = qs doc "#add"
let add_btn = qs add_div "button"
let add_key = to_i (qs add_div ".key")
let add_value = to_i (qs add_div ".value")

let remove_div = qs doc "#remove"
let remove_key = to_i (qs remove_div ".key")
let remove_btn = qs remove_div "button"

let _ = 
  Lwt_js_events.(
    async_loop
      click 
      remove_btn
      (fun _ _ ->
        let key = String.trim (Js.to_string remove_key##.value) in
        let _ = WebStorage.Local.remove key in
        Lwt.return_unit
      )
  )

let _ = 
  Lwt_js_events.(
    async_loop 
      click 
      add_btn 
      (fun _ _ ->
        let k = String.trim (Js.to_string add_key##.value) in 
        let v = String.trim (Js.to_string add_value##.value) in 
        let _ = 
          if (String.length k) > 0 && (String.length v) > 0 
          then WebStorage.Local.set k v
          else alert "Key and value must be filled"
        in Lwt.return_unit
      )
  )

let _ = 
  Lwt_js_events.(
    async_loop
      click
      clear_btn
      (fun _ _ ->
        let _ = puts "Clear storage" in 
        let _ = WebStorage.Local.clear () in
        Lwt.return_unit
      )
  )
