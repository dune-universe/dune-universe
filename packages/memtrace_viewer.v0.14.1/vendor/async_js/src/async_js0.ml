open Core_kernel
module Time_ns = Core_kernel.Time_ns
module Clock_ns = Async_kernel.Clock_ns
module Scheduler = Async_kernel.Async_kernel_scheduler.Private
open Js_of_ocaml

let sleep d = Clock_ns.after (Time_ns.Span.of_sec d)
let yield () = Scheduler.yield (Scheduler.t ())

let extract_js_error = ref (fun _ -> None)

let run =
  let module State = struct
    type t =
      | Idle
      | Running
      | Will_run_soon
  end
  in
  let module Next_wakeup = struct
    type t =
      | At of Time_ns.t * float
      | No_wakeup
      | Soon
  end
  in
  let state = ref State.Idle in
  let timeouts = Stack.create () in
  let run_after ~f ~ms = ignore (Dom_html.setTimeout f ms : Dom_html.timeout_id_safe) in
  let rec loop () =
    let t = Scheduler.t () in
    match !state, Scheduler.uncaught_exn t with
    | _, Some _ | State.Running, None -> ()
    | (State.Idle | State.Will_run_soon), None ->
      state := State.Running;
      Scheduler.run_cycle t;
      let next_wakeup : Next_wakeup.t =
        if Scheduler.can_run_a_job t
        then Soon
        else (
          match Scheduler.next_upcoming_event t with
          | None -> No_wakeup
          | Some next ->
            let now = Time_ns.now () in
            let d = Time_ns.diff next now in
            let d_ms = Time_ns.Span.to_ms d in
            if Float.( <= ) d_ms 0. then Soon else At (next, d_ms))
      in
      Option.iter (Scheduler.uncaught_exn_unwrapped t) ~f:(fun (exn, _sexp) ->
        match Async_kernel.Monitor.extract_exn exn with
        | Js.Error err -> Js.raise_js_error err
        | exn ->
          (match !extract_js_error exn with
           | None -> raise exn
           | Some err ->
             (* Hack to get a better backtrace *)
             (* We first output the stringified ocaml exception *)
             Firebug.console##error (Js.string (Exn.to_string exn));
             (* And then raise the embedded javascript error that provides a proper
                backtrace with good sourcemap support.
                The name of this javascript error is probably not meaningful which is why
                we first output the serialization of ocaml exception. *)
             Js.raise_js_error err));
      (match next_wakeup with
       | No_wakeup -> state := Idle
       | Soon ->
         state := Will_run_soon;
         run_after ~f:loop ~ms:0.
       | At (at, d_ms) ->
         state := Idle;
         if Stack.is_empty timeouts || Time_ns.( < ) at (Stack.top_exn timeouts)
         then (
           Stack.push timeouts at;
           run_after ~f:run_timeout ~ms:d_ms))
  and run_timeout () =
    (* Each call to [run_timeout] removes exactly one element from [timeouts].  This
       maintains the invariant that [Stack.length timeouts] is exactly the number of
       outstanding timeouts we have registered. *)
    ignore (Stack.pop_exn timeouts : Time_ns.t);
    loop ()
  in
  fun () ->
    match !state with
    | State.Idle ->
      run_after ~f:loop ~ms:0.;
      state := State.Will_run_soon
    | State.Running | State.Will_run_soon -> ()
;;

let log name exn =
  let exn =
    match Async_kernel.Monitor.extract_exn exn with
    | Js.Error err -> `Js err
    | exn ->
      (match !extract_js_error exn with
       | None -> `Exn exn
       | Some err -> `Js_and_exn (exn, err))
  in
  match exn with
  | `Js err -> Firebug.console##error_2 (Js.string name) err
  | `Exn exn -> Firebug.console##error_2 (Js.string name) (Js.string (Exn.to_string exn))
  | `Js_and_exn (exn, err) ->
    Firebug.console##error_3 (Js.string name) (Js.string (Exn.to_string exn)) err
;;

let initialized_ref = ref false

let initialization =
  lazy
    (let t = Scheduler.t () in
     initialized_ref := true;
     Scheduler.set_job_queued_hook t (fun _ -> run ());
     Scheduler.set_event_added_hook t (fun _ -> run ());
     Scheduler.set_thread_safe_external_job_hook t run;
     Async_kernel.Monitor.Expert.try_with_log_exn := log "Async_kernel: Monitor.try_with";
     Async_kernel.Monitor.detach_and_iter_errors
       Async_kernel.Monitor.main
       ~f:(log "Async_kernel: Unhandled exception");
     run ())
;;

let init () = force initialization
let initialized () = !initialized_ref
let set_extract_js_error f = extract_js_error := f

let document_loaded =
  let js_string_compare s =
    let compare_using_javascript_triple_equal_for_strings = phys_equal in
    compare_using_javascript_triple_equal_for_strings (Js.string s)
  in
  let ready_state_change = "readystatechange" in
  let complete = "complete" in
  let readystatechange_ev = Dom.Event.make ready_state_change in
  let add_event target evt handler =
    ignore
      (Dom_html.addEventListener target evt handler Js._false : Dom.event_listener_id)
  in
  fun () ->
    if js_string_compare complete Dom_html.document##.readyState
    then Async_kernel.Deferred.unit
    else (
      let loaded = Async_kernel.Ivar.create () in
      let handler evt =
        if (not (js_string_compare ready_state_change evt##._type))
        || js_string_compare complete Dom_html.document##.readyState
        then Async_kernel.Ivar.fill_if_empty loaded ();
        Js._true
      in
      add_event Dom_html.document Dom_html.Event.domContentLoaded (Dom.handler handler);
      add_event Dom_html.document readystatechange_ev (Dom.handler handler);
      add_event Dom_html.window Dom_html.Event.load (Dom.handler handler);
      Async_kernel.Ivar.read loaded)
;;
