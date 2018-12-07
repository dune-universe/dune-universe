open Ctypes
open Foreign

module Stub = struct
  type t= unit ptr
  let t: t typ= ptr void

  let libopencc= Dl.(dlopen ~filename:"libopencc.so.1" ~flags:[RTLD_NOW])

  let create=
    foreign ~from:libopencc "opencc_open"
    (string @-> returning t)

  let close=
    foreign ~from:libopencc "opencc_close"
    (t @-> returning int)

  let convert_utf8=
    foreign ~from:libopencc "opencc_convert_utf8"
    (t @-> string @-> returning string)

  let set_conversion_mode=
    foreign ~from:libopencc "opencc_set_conversion_mode"
    (t @-> int @-> returning void)

  let perror=
    foreign ~from:libopencc "opencc_perror"
    (string @-> returning void)
end

type t= Stub.t

type conversion=
  | Fast
  | SegmentOnly
  | ListCandidates

let create cfg=
  let oc= Stub.create cfg in
  Gc.finalise
    (fun oc-> match Stub.close oc with
    | 0-> ()
    | _-> Stub.perror "")
    oc;
  oc

let convert_utf8= Stub.convert_utf8

let set_conversion_mode t mode=
  match mode with
  | Fast-> Stub.set_conversion_mode t 0
  | SegmentOnly-> Stub.set_conversion_mode t 1
  | ListCandidates-> Stub.set_conversion_mode t 2

