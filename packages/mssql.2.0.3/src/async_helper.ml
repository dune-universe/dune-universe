open Async_unix

(* Run a function in async without the caller needing to keep track.
   If we're already in Async's main thread, just run it, otherwise
   do cross-thread magic *)
let safely_run_in_async f =
  if Thread_safe.am_holding_async_lock () then f () else Thread_safe.run_in_async_exn f
;;
