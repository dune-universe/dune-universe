open Ezjs_min_lwt.Promise
open Storage_local

let to_lwt_cb_opt0 f callback =
  let waiter, notifier = Lwt.wait () in
  let callback = match callback with
   | Some cb -> cb (Lwt.wakeup notifier)
   | _ -> Lwt.wakeup notifier in
  f callback;
  waiter

let get ?key _st = to_lwt_cb0 (fun cb -> get ?key _st cb)
let get_arr ?keys _st = to_lwt_cb0 (fun cb -> get_arr ?keys _st cb)
let get_o ?obj _st = to_lwt_cb0 (fun cb -> get_o ?obj _st cb)
let set ?callback _st o =
  to_lwt_cb_opt0 (fun callback -> set ~callback _st o) callback
let remove ?callback _st s =
  to_lwt_cb_opt0 (fun callback -> remove ~callback _st s) callback
let clear ?callback _st =
  to_lwt_cb_opt0 (fun callback -> clear ~callback _st) callback

let sync = ()
let local = ()
