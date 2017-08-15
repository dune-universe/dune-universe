(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

(* Lwt connection multiplexer *)
open Lwt
open Result

module type RPC = sig
  type transport
  type id
  type request_hdr
  type request_body
  type response_hdr
  type response_body

  val recv_hdr : transport -> (id option * response_hdr) Lwt.t
  val recv_body : transport -> request_hdr -> response_hdr -> response_body -> (unit, Protocol.Error.t) result Lwt.t
  val send_one : transport -> request_hdr -> request_body -> unit Lwt.t
  val id_of_request : request_hdr -> id
  val handle_unrequested_packet : transport -> response_hdr -> unit Lwt.t
end


module Make = functor (R : RPC) -> struct
  exception Unexpected_id of R.id
  exception Shutdown

  type client = {
    transport : R.transport;
    outgoing_mutex: Lwt_mutex.t;
    id_to_wakeup : (R.id, R.request_hdr * ((unit, Protocol.Error.t) result Lwt.u) * R.response_body) Hashtbl.t;
    mutable dispatcher_thread : unit Lwt.t;
    mutable dispatcher_shutting_down : bool;
  }

  let rec dispatcher t =
    let th = Lwt.catch
      (fun () ->
        R.recv_hdr t.transport
        >>= fun (id, pkt) ->
        match id with
        | None -> R.handle_unrequested_packet t.transport pkt
        | Some id ->
          if not(Hashtbl.mem t.id_to_wakeup id)
          then fail (Unexpected_id id)
          else begin
            let request_hdr, waker, response_body = Hashtbl.find t.id_to_wakeup id in
            R.recv_body t.transport request_hdr pkt response_body
            >>= fun response ->
            Lwt.wakeup waker response;
            Hashtbl.remove t.id_to_wakeup id;
            Lwt.return ()
          end
      ) (fun e ->
        t.dispatcher_shutting_down <- true;
        Hashtbl.iter (fun _ (_,u, _) -> Lwt.wakeup_later_exn u e) t.id_to_wakeup;
        fail e)
    in th >>= fun () -> dispatcher t

let rpc req_hdr req_body response_body t =
  let sleeper, waker = Lwt.wait () in
  if t.dispatcher_shutting_down
  then fail Shutdown
  else begin
    let id = R.id_of_request req_hdr in
    Hashtbl.add t.id_to_wakeup id (req_hdr, waker, response_body);
    Lwt_mutex.with_lock t.outgoing_mutex
      (fun () -> R.send_one t.transport req_hdr req_body)
    >>= fun () ->
    sleeper
end

let create transport =
  let t = {
    transport = transport;
    outgoing_mutex = Lwt_mutex.create ();
    id_to_wakeup = Hashtbl.create 10;
    dispatcher_thread = Lwt.return ();
    dispatcher_shutting_down = false; } in
  t.dispatcher_thread <- dispatcher t;
  Lwt.return t

end
