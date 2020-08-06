(** Minimal binding of http://davidshimjs.github.io/qrcodejs/ *)

module Js = Js_of_ocaml.Js

class type qrcode  = object
  method makeCode : Js.js_string Js.t -> unit Js.meth
  method clear : unit Js.meth
end

type qrcode_cs = (Js.js_string Js.t -> Js.js_string Js.t -> qrcode Js.t) Js.constr

let make_code id text =
  let qrcode_ctsr : qrcode_cs = Js.Unsafe.global##._QRCode in
  new%js qrcode_ctsr (Js.string id) (Js.string text)

let update_code qrcode text = qrcode##makeCode (Js.string text)
let clear_code qrcode = qrcode##clear
