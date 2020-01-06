let init_session monitor=
  let mbox= Lwt_mvar.create_empty () in
  let callback events=
    Lwt_preemptive.run_in_main (fun ()-> Lwt_mvar.put mbox events)
  in
  (Fswatch.init_session monitor callback, mbox)

let start_monitor handle ()=
  Lwt_preemptive.detach Fswatch.start_monitor handle
