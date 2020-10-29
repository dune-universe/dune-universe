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

external solo5_yield : Time.t -> int64 = "mirage_solo5_yield_2"

(* A Map from Int64 (solo5_handle_t) to an Lwt_condition. *)
module HandleMap = Map.Make(Int64)
let work = ref HandleMap.empty

(* Wait for work on handle [h]. The Lwt_condition and HandleMap binding are
 * created lazily the first time [h] is waited on. *)
let wait_for_work_on_handle h =
  match HandleMap.find h !work with
    | exception Not_found ->
      let cond = Lwt_condition.create () in
      work := HandleMap.add h cond !work;
      Lwt_condition.wait cond
    | cond ->
      Lwt_condition.wait cond

(* Execute one iteration and register a callback function *)
let run t =
  let rec aux () =
    Lwt.wakeup_paused ();
    Time.restart_threads Time.time;
    match Lwt.poll t with
    | Some () ->
        ()
    | None ->
        (* Call enter hooks. *)
        Mirage_runtime.run_enter_iter_hooks () ;
        let timeout =
          match Time.select_next () with
          |None -> Int64.add (Time.time ()) (Duration.of_day 1)
          |Some tm -> tm
        in
        let ready_set = solo5_yield timeout in
        if not (Int64.equal 0L ready_set) then begin
          (* Some I/O is possible, wake up threads and continue. *)
          let is_in_set set x =
            not Int64.(equal 0L (logand set (shift_left 1L (to_int x)))) in
          HandleMap.iter (fun k v ->
            if is_in_set ready_set k then Lwt_condition.broadcast v ()) !work
        end;
        (* Call leave hooks. *)
        Mirage_runtime.run_leave_iter_hooks () ;
        aux ()
  in
  aux ()

let () =
  at_exit (fun () ->
    Lwt.abandon_wakeups () ;
    run (Mirage_runtime.run_exit_hooks ()))
