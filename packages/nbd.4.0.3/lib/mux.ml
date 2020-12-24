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

(** Lwt connection multiplexer. Multiplexes between parallel requests from
    multiple clients over a single output channel to a server that may send
    responses out of order. Each request and response carries an [id] that is
    used to match responses to requests. *)

open Lwt.Infix
open Result

module type RPC = sig
  type transport
  (** The transport mechanism used to send and receive messages *)

  type id
  (** Each [request_hdr] and [response_hdr] carries an [id] that is used to
      match responses to requests. *)

  type request_hdr
  type request_body
  type response_hdr
  type response_body

  val recv_hdr : transport -> (id option * response_hdr) Lwt.t

  val recv_body : transport -> request_hdr -> response_hdr -> response_body -> (unit, Protocol.Error.t) result Lwt.t
  (** [recv_body transport request_hdr response_hdr response_body] returns [Ok ()]
      and receives and writes the body of the response into [response_body] if
      the request has been successful, otherwise returns an [Error]. The
      [request_hdr] parameter is the output of a preceding [recv_hdr] call. *)

  val send_one : transport -> request_hdr -> request_body -> unit Lwt.t
  (** Send a single request. Invocations of this function will not be interleaved
      because they are protected by a mutex *)

  val id_of_request : request_hdr -> id
  val handle_unrequested_packet : transport -> response_hdr -> unit Lwt.t
end


module Make (R : RPC) : sig
  type client

  val rpc : R.request_hdr -> R.request_body -> R.response_body -> client -> (unit, Protocol.Error.t) Result.result Lwt.t
  (** [rpc req_hdr req_body response_body client] sends a request to the server, and
      saves the response into [response_body]. Will block until a response to
      this request is received from the server. *)

  val create : R.transport -> client Lwt.t
  (** [create transport] creates a new client that manages parallel requests
      over the given transport channel. All communication over this channel
      must go through the returned client. *)

end = struct
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
             then Lwt.fail (Unexpected_id id)
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
            Lwt.fail e)
    in th >>= fun () -> dispatcher t

  let rpc req_hdr req_body response_body t =
    let sleeper, waker = Lwt.wait () in
    if t.dispatcher_shutting_down
    then Lwt.fail Shutdown
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
