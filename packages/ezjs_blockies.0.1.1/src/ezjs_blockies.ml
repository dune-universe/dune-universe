open Js_of_ocaml
open Js

class type blockies_options = object
  method seed : js_string t optdef readonly_prop
  method color : js_string t optdef readonly_prop
  method bgcolor : js_string t optdef readonly_prop
  method size : int optdef readonly_prop
  method scale : int optdef readonly_prop
  method spotcolor : js_string t optdef readonly_prop
end

class type blockies = object
  method create : blockies_options t -> Dom_html.canvasElement t meth
  method render : blockies_options t -> Dom_html.canvasElement t -> unit meth
end

let optdef f = function
  | None -> undefined
  | Some x -> def (f x)

let make_opts ?seed ?color ?bgcolor ?size ?scale ?spotcolor () : blockies_options t =
  object%js
    val seed = optdef string seed
    val color = optdef string color
    val bgcolor = optdef string bgcolor
    val size = optdef (fun x -> x) size
    val scale = optdef (fun x -> x) scale
    val spotcolor = optdef string spotcolor
  end

let blockies () : blockies t = Unsafe.variable "blockies"
let blockies_opt () : blockies t option = Optdef.to_option @@ Unsafe.variable "blockies"

let ready ?(not_found=fun () -> ()) ?(timeout=500.) f =
  match blockies_opt () with
  | None ->
    let cb () = match blockies_opt () with
      | None -> not_found ()
      | Some b -> f b in
    ignore @@ Dom_html.window##setTimeout (wrap_callback cb) timeout
  | Some b -> f b

let create ?seed ?color ?bgcolor ?size ?scale ?spotcolor () =
  let options = make_opts ?seed ?color ?bgcolor ?size ?scale ?spotcolor () in
  (blockies ())##create options

let render ?seed ?color ?bgcolor ?size ?scale ?spotcolor canvas =
  let options = make_opts ?seed ?color ?bgcolor ?size ?scale ?spotcolor () in
  (blockies ())##render options canvas
