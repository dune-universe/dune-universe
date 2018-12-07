open Ctypes
open Foreign

module Stub = struct
  type t= unit ptr
  let t: t typ= ptr void

  let libopencc= Dl.(dlopen ~filename:"libopencc.so.2" ~flags:[RTLD_NOW])

  let create=
    foreign ~from:libopencc "opencc_open"
    (string @-> returning t)

  let close=
    foreign ~from:libopencc "opencc_close"
    (t @-> returning int)

  let convert_utf8=
    foreign ~from:libopencc "opencc_convert_utf8"
    (t @-> string @-> int @-> returning string)

  let error=
    foreign ~from:libopencc "opencc_error"
    (void @-> returning string)
end

type t= Stub.t

let create cfg=
  let oc= Stub.create cfg in
  Gc.finalise
    (fun oc-> match Stub.close oc with
    | 0-> ()
    | _-> Printf.eprintf "%s\n" (Stub.error ()))
    oc;
  oc

let convert_utf8 t str= Stub.convert_utf8 t str (String.length str)
let error= Stub.error

