(* Lightweight thread library for Objective Caml
 * http://www.ocsigen.org/lwt
 * Module Lwt_main
 * Copyright (C) 2009 Jérémie Dimino
 * Copyright (C) 2010 Anil Madhavapeddy <anil@recoil.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, with linking exceptions;
 * either version 2.1 of the License, or (at your option) any later
 * version. See COPYING file for details.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *)

external block_domain : [`Time] Time.Monotonic.t -> unit = "caml_block_domain"

let evtchn = Eventchn.init ()

external look_for_work: unit -> bool = "stub_evtchn_look_for_work"

(* Execute one iteration and register a callback function *)
let run t =
  let rec aux () =
    Lwt.wakeup_paused ();
    Time.restart_threads Time.Monotonic.time;
    match Lwt.poll t with
    | Some () ->
        ()
    | None ->
        if look_for_work () then begin
          (* Some event channels have triggered, wake up threads
           * and continue without blocking. *)
          (* Call enter hooks. *)
          Mirage_runtime.run_enter_iter_hooks () ;
          Activations.run evtchn;
          (* Call leave hooks. *)
          Mirage_runtime.run_leave_iter_hooks () ;
          aux ()
        end else begin
          let timeout =
            match Time.select_next () with
            |None -> Time.Monotonic.(time () + of_nanoseconds 86_400_000_000_000L) (* one day = 24 * 60 * 60 s *)
            |Some tm -> tm
          in
          MProf.Trace.(note_hiatus Wait_for_work);
          block_domain timeout;
          MProf.Trace.note_resume ();
          aux ()
        end in
  aux ()

let () =
  at_exit (fun () ->
    Lwt.abandon_wakeups () ;
    run (Mirage_runtime.run_exit_hooks ()))
