open Core
open Async

let create ?start_on ~check_interval ~change_interval on_failure =
  let world = ref 0 in
  let rec update_loop () =
    upon (after change_interval) (fun _t ->
        world := !world + 1;
        update_loop ()
      )
  in
  let start_watcher_thread () =
    ignore (Thread.create (fun () ->
        let rec loop () =
          let oldworld = !world in
          Time.pause check_interval;
          if oldworld = !world then on_failure ();
          loop ()
        in
        loop ()
      ) ())
  in
  update_loop ();
  match start_on with
  | None -> start_watcher_thread ()
  | Some d -> upon d start_watcher_thread
;;
