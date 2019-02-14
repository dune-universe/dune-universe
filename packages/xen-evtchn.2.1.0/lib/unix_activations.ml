(* Copyright (C) Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*)

(* The high-level interface creates one counter per event channel port.
   Every time the system receives a notification it increments the counter.
   Threads which have blocked elsewhere call 'after' which blocks until
   the stored counter is greater than the value they have already -- so
   if an event comes in between calls then it will not be lost.

   In the high-level interface it's almost impossible to miss an event.
   The only way you can miss is if you block while your port's counter
   wraps. Arguably if you have failed to notice 2bn (32-bit) wakeups then
   you have bigger problems. *)

open Lwt.Infix

let nr_events = 1024

type event = int

let program_start = min_int

type port = {
  mutable counter: event;
  c: unit Lwt_condition.t;
}

external fd: Eventchn.handle -> Unix.file_descr = "stub_evtchn_fd"
external pending: Eventchn.handle -> Eventchn.t = "stub_evtchn_pending"

let ports = Array.init nr_events (fun _ -> { counter = program_start; c = Lwt_condition.create () })

let event_cb = Array.init nr_events (fun _ -> Lwt_dllist.create ())

let dump () =
  Printf.printf "Number of received event channel events:\n";
  for i = 0 to nr_events - 1 do
    if ports.(i).counter <> program_start
    then Printf.printf "port %d: %d\n%!" i (ports.(i).counter - program_start)
  done

let wake port =
  let port = Eventchn.to_int port in
  Lwt_dllist.iter_node_l (fun node ->
      let u = Lwt_dllist.get node in
      Lwt_dllist.remove node;
      Lwt.wakeup_later u ();
    ) event_cb.(port);
  ports.(port).counter <- ports.(port).counter + 1;
  Lwt_condition.broadcast ports.(port).c ()

(* Go through the event mask and activate any events, potentially spawning
   new threads *)
let run_real xe =
  let fd = Lwt_unix.of_unix_file_descr ~blocking:false ~set_flags:true (fd xe) in
  let rec inner () =
    Lwt_unix.wait_read fd
    >>= fun () ->
    let port = pending xe in
    wake port;
    Eventchn.unmask xe port;
    inner ()
  in inner ()

let activations_thread_started = ref false

let start_activations_thread () =
  if not !activations_thread_started then begin
    activations_thread_started := true;
    let (_: unit Lwt.t) = run_real (Eventchn.init ()) in
    ()
  end

let after evtchn counter =
  (* This will print an error to stderr if we have no event channels *)
  start_activations_thread ();
  let port = Eventchn.to_int evtchn in
  let rec loop () =
    if ports.(port).counter <= counter && (Eventchn.is_valid evtchn) then begin
      Lwt_condition.wait ports.(port).c
      >>= fun () ->
      loop ()
    end else Lwt.return_unit in
  loop ()
  >>= fun () ->
  if Eventchn.is_valid evtchn
  then Lwt.return ports.(port).counter
  else Lwt.fail Generation.Invalid

let wait port =
  (* This will print an error to stderr if we have no event channels *)
  start_activations_thread ();
  let th, u = Lwt.task () in
  let node = Lwt_dllist.add_r u event_cb.(Eventchn.to_int port) in
  Lwt.on_cancel th (fun _ -> Lwt_dllist.remove node);
  th

(* Here for backwards compatibility *)
let run _ = ()

