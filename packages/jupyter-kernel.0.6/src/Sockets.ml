(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

open Lwt.Infix

let context = ZMQ.Context.create ()
let () = at_exit
    (fun () -> ZMQ.Context.terminate context)

let addr conn port =
  Protocol_j.(conn.transport ^ "://" ^ conn.ip ^ ":" ^ string_of_int port)

let open_socket typ conn port =
  let socket = ZMQ.Socket.create context typ in
  let addr = addr conn port in
  let () = ZMQ.Socket.bind socket addr in
  Log.debug (fun k->k "open and bind socket %s" addr);
  Lwt_zmq.Socket.of_socket socket

let close_socket s =
  let s = Lwt_zmq.Socket.to_socket s in
  ZMQ.Socket.close s

type t = {
  shell : [`Router] Lwt_zmq.Socket.t;
  control : [`Router] Lwt_zmq.Socket.t;
  stdin : [`Router] Lwt_zmq.Socket.t;
  iopub : [`Pub] Lwt_zmq.Socket.t;
  heartbeat: [`Rep ] Lwt_zmq.Socket.t;
}

let open_sockets conn =
  let open Protocol_j in
  Log.debug (fun k->k "open sockets `%s`" (string_of_connection_info conn));
  { shell = open_socket ZMQ.Socket.router conn conn.shell_port;
    control = open_socket ZMQ.Socket.router conn conn.control_port;
    stdin = open_socket ZMQ.Socket.router conn conn.stdin_port;
    iopub = open_socket ZMQ.Socket.pub conn conn.iopub_port;
    heartbeat = open_socket ZMQ.Socket.rep conn conn.hb_port;
  }

let close_sockets (t:t) : unit Lwt.t =
  close_socket t.shell;
  close_socket t.control;
  close_socket t.stdin;
  close_socket t.iopub;
  close_socket t.heartbeat;
  Lwt.return_unit

let heartbeat (t:t) =
  Log.debug (fun k->k "listening for hearbeat requests");
  let rec loop() =
    Message.wrap_retry Lwt_zmq.Socket.recv t.heartbeat >>= fun data ->
    Log.debug (fun k->k "Heartbeat received");
    Message.wrap_retry (Lwt_zmq.Socket.send t.heartbeat) data >>= fun () ->
    loop()
  in
  loop () >>= fun () ->
  (* XXX close down properly...we never get here *)
  ZMQ.Socket.close (Lwt_zmq.Socket.to_socket t.heartbeat);
  Lwt.return ()

let dump name socket =
  let rec loop () =
    Message.wrap_retry Message.recv socket >>= fun msg ->
    let () = Message.log name msg in
    loop()
  in
  loop()
