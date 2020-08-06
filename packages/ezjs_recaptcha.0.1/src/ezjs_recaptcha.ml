module Js = Js_of_ocaml.Js

class type input = object
  method action : Js.js_string Js.t Js.prop
end

class type result = object
  method _then : (Js.js_string Js.t -> unit) -> unit Js.meth
end

class type param = object
  method sitekey : Js.js_string Js.t Js.prop
  method callback : (Js.js_string Js.t -> unit) Js.prop
  method theme : Js.js_string Js.t Js.prop
  method size : Js.js_string Js.t Js.prop
end

class type grecaptcha  = object
  method ready : (unit -> unit) -> unit Js.meth
  method execute : Js.js_string Js.t -> input Js.t -> result Js.t Js.meth
  method render : Js.js_string Js.t -> param Js.t -> unit Js.meth
end

let recaptcha : grecaptcha Js.t = Js.Unsafe.variable "grecaptcha"

module V3 = struct
  let check ?(action="login") site_key f =
    recaptcha##ready (fun () ->
        let a : input Js.t = Js.Unsafe.obj [||] in
        a##.action := Js.string action ;
        let res = recaptcha##execute (Js.string site_key) a in
        res##_then(fun token -> f (Js.to_string token)))
end

module V2 = struct

  type theme = Light | Dark
  type data_size = Compact | Normal

  let theme_to_string = function Light -> "light" | Dark -> "dark"
  let data_size_to_string = function Normal -> "normal" | Compact -> "compact"

  let check ?(theme=Light) ?(data_size=Normal) elt_id site_key handler =
    let elt_id = Js.string elt_id in
    let parameters : param Js.t = Js.Unsafe.obj [||] in
    parameters##.sitekey := Js.string site_key ;
    parameters##.callback :=
      (fun token -> handler @@ Js.to_string token);
    parameters##.theme := Js.string @@ theme_to_string theme ;
    parameters##.size := Js.string @@ data_size_to_string data_size ;

    Js.Unsafe.global##.onloadCallback :=
      (fun () -> recaptcha##render elt_id parameters)
end
