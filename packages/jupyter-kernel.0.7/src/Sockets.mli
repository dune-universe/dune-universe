(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

(** {1 Sockets to Jupyter}

    See https://jupyter-client.readthedocs.io/en/latest/messaging.html *)

type t = {
  shell : [`Router] Zmq_lwt.Socket.t;
  control : [`Router] Zmq_lwt.Socket.t;
  stdin : [`Router] Zmq_lwt.Socket.t;
  iopub : [`Pub] Zmq_lwt.Socket.t;
  heartbeat: [`Rep ] Zmq_lwt.Socket.t;
}

val open_sockets : Protocol_t.connection_info -> t

val close_sockets : t -> unit Lwt.t

val heartbeat : t -> unit Lwt.t

val dump : string -> [`Router] Zmq_lwt.Socket.t -> unit Lwt.t
