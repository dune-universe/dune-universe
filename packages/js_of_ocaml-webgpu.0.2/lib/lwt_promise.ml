open Js_of_ocaml
open Promise

let to_lwt (p : 'a Promise.t Js.t) : 'a Lwt.t =
  let r, w = Lwt.task () in
  ignore
    ((Js.Unsafe.coerce p)##_then
       (Js.wrap_callback (fun v -> Lwt.wakeup w v))
       (Js.wrap_callback (fun e ->
            Lwt.wakeup_exn
              w
              (try raise e with
              | e -> e)))
      : unit);
  r
;;

let of_lwt (p : unit -> 'a Lwt.t) : 'a Promise.t Js.t =
  new%js _Promise
    (Js.wrap_callback (fun resolve reject ->
         Lwt.try_bind
           p
           (fun v -> Js.Unsafe.fun_call resolve [| Js.Unsafe.inject v |])
           (fun e -> Js.Unsafe.fun_call reject [| Js.Unsafe.inject e |])))
;;
