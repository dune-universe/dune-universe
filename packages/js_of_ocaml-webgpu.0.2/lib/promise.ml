open Js_of_ocaml

type 'a t

let _Promise = Js.Unsafe.global##._Promise

let _catch (p : 'a t Js.t) (f : _ -> 'a t Js.t) : 'a t Js.t =
  (Js.Unsafe.coerce p)##_catch (Js.wrap_callback f)
;;

let _then
    (p : 'a t Js.t)
    ?(catch : (_ -> 'b t Js.t) option)
    (f : 'a -> 'b t Js.t)
    : 'b t Js.t
  =
  match catch with
  | None -> (Js.Unsafe.coerce p)##_then (Js.wrap_callback f)
  | Some catch ->
    (Js.Unsafe.coerce p)##_then
      (Js.wrap_callback f)
      (Js.wrap_callback catch)
;;

let resolve_value (v : 'a) : 'a t Js.t =
  Js.Unsafe.meth_call _Promise "resolve" [| Js.Unsafe.inject v |]
;;

let resolve_promise (v : 'a t Js.t) : 'a t Js.t =
  Js.Unsafe.meth_call _Promise "resolve" [| Js.Unsafe.inject v |]
;;

let reject_value (v : 'a) : 'a t Js.t =
  Js.Unsafe.meth_call _Promise "reject" [| Js.Unsafe.inject v |]
;;

let reject_promise (v : 'a t Js.t) : 'a t Js.t =
  Js.Unsafe.meth_call _Promise "rejetct" [| Js.Unsafe.inject v |]
;;

let race (promise_list : 'a t Js.t Js.js_array Js.t) : 'a t Js.t =
  Js.Unsafe.meth_call _Promise "race" [| Js.Unsafe.inject promise_list |]
;;

let all (promise_list : 'a t Js.t Js.js_array Js.t) : 'a t Js.t =
  Js.Unsafe.meth_call _Promise "all" [| Js.Unsafe.inject promise_list |]
;;
