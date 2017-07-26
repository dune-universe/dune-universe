(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
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

open Lwt
open Printf

exception Shutdown

module Front = struct

  type ('a, 'b) t = {
    ring: ('a, 'b) Ring.Rpc.Front.t;
    wakers: ('b, 'a Lwt.u) Hashtbl.t; (* id * wakener *)
    waiters: unit Lwt.u Lwt_sequence.t;
    string_of_id: 'b -> string;
  }

  let init string_of_id ring =
    let wakers = Hashtbl.create 7 in
    let waiters = Lwt_sequence.create () in
    { ring; wakers; waiters; string_of_id }

  let rec get_free_slot t =
    if Ring.Rpc.Front.get_free_requests t.ring > 0 then
      return (Ring.Rpc.Front.next_req_id t.ring)
    else begin
      let th, u = MProf.Trace.named_task "ring.get_free_slot" in
      let node = Lwt_sequence.add_r u t.waiters in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      th >>= fun () ->
      get_free_slot t
    end 

  let rec wait_for_free t n =
    if Ring.Rpc.Front.get_free_requests t.ring >= n then
      return ()
    else begin
      assert (n <= Ring.Rpc.Front.nr_ents t.ring);
      let th, u = MProf.Trace.named_task "ring.wait_for_free" in
      let node = Lwt_sequence.add_r u t.waiters in
      Lwt.on_cancel th (fun _ -> Lwt_sequence.remove node);
      th >>= fun () ->
      wait_for_free t n
    end

  let poll t respfn =
    Ring.Rpc.Front.ack_responses t.ring (fun slot ->
        MProf.Trace.label "ring.poll ack_response";
        let id, resp = respfn slot in
        try
          let u = Hashtbl.find t.wakers id in
          Hashtbl.remove t.wakers id;
          Lwt.wakeup u resp
        with Not_found ->
          printf "RX: ack (id = %s) wakener not found\n" (t.string_of_id id);
          printf "    valid ids = [ %s ]\n%!"
            (String.concat "; " 
               (List.map t.string_of_id 
                  (Hashtbl.fold (fun k _ acc -> k :: acc) t.wakers [])));
      );
    (* Check for any sleepers waiting for free space *)
    match Lwt_sequence.take_opt_l t.waiters with
    |None -> ()
    |Some u -> Lwt.wakeup u ()

  let write t reqfn =
    get_free_slot t
    >>= fun slot_id ->
    let slot = Ring.Rpc.Front.slot t.ring slot_id in
    let th, u = MProf.Trace.named_task "ring.write" in
    let id = reqfn slot in
    Lwt.on_cancel th (fun _ -> Hashtbl.remove t.wakers id);
    Hashtbl.add t.wakers id u;
    return th

  let push t notifyfn =
    if Ring.Rpc.Front.push_requests_and_check_notify t.ring
    then notifyfn ()

  let push_request_and_wait t notifyfn reqfn =
    write t reqfn
    >>= fun th ->
    push t notifyfn;
    th

  let push_request_async t notifyfn reqfn freefn =
    write t reqfn
    >>= fun th ->
    push t notifyfn;
    let _ = freefn th in
    return ()

  let shutdown t =
    Hashtbl.iter (fun _id th ->
        Lwt.wakeup_exn th Shutdown
      ) t.wakers;
    (* Check for any sleepers waiting for free space *)
    let rec loop () =
      match Lwt_sequence.take_opt_l t.waiters with
      | None -> ()
      | Some u -> Lwt.wakeup_exn u Shutdown; loop ()
    in loop ()

  let to_string t = Ring.Rpc.Front.to_string t.ring
end

module Back = struct
  type ('a, 'b) t = {
    ring: ('a, 'b) Ring.Rpc.Back.t;
    string_of_id: 'b -> string;
  }

  let init string_of_id ring =
    { ring; string_of_id }

  let push_response t notifyfn rspfn =
    let slot_id = Ring.Rpc.Back.next_res_id t.ring in
    let slot = Ring.Rpc.Back.slot t.ring slot_id in
    rspfn slot;
    if Ring.Rpc.Back.push_responses_and_check_notify t.ring
    then notifyfn ()

end
