(** A throttle that coalesces jobs, only fully executing the most recent job. *)
open Async

type 'a t = {
  mutable id : int;
  mutable next_result : 'a Ivar.t;
  throttle : unit Throttle.t;
}

let create () = {
  id = 0;
  next_result = Ivar.create ();
  throttle = Throttle.create ~continue_on_error:true ~max_concurrent_jobs:1;
}

let enqueue t job =
  t.id <- t.id + 1;
  let my_id = t.id in
  let job_complete = Ivar.create () in
  (* The double-deferred is so that we can execute only one job concurrently,
     but allow previous jobs to get the result of the most recent job. *)
  let maybe_abort ~continue =
    if t.id <> my_id
    then
      begin
        if not (Ivar.is_full job_complete)
        then Ivar.fill job_complete ();
        Ivar.read t.next_result
      end
    else
      continue ()
      >>| fun result ->
        if not (Ivar.is_full job_complete)
        then Ivar.fill job_complete ();
        result
  in
  let run_job () =
    let result = job ~maybe_abort in
    (Ivar.read job_complete)
    >>| fun () -> result
  in
  Throttle.enqueue t.throttle run_job
  >>= fun result ->
    result
  >>| fun result ->
     if t.id = my_id then begin
       Ivar.fill t.next_result result;
       t.next_result <- Ivar.create ();
     end;
     result
;;

