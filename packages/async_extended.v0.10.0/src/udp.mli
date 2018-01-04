(* provides an interface to UDP socket setup and teardown that automatically handles
  many of the details.  Most importantly:

  - addresses will be looked up and/or converted from string representations automagically
  - sockets will be closed when possible on error conditions
  - unlike the low level unix functions these collect errors and throw them as exceptions
*)

open! Core
open! Async

(** [udp_server addr port f] except that f is called on every packet received with the
    address it was received from and the data received. The next packet will not be read
    until the Deferred returned by f is determined *)
val udp_server : addr:string
  -> port:int
  -> f:(Socket.Address.Inet.t -> string -> unit Deferred.t)
  -> unit Deferred.t

(** [udp_client ~addr ~port ~f] Same as tcp_client for udp *)
val udp_client : addr:string
  -> port:int
  -> f:(Writer.t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [connect socket_type ~addr] connects a socket to addr *)
val connect :
  ([< `Inet of Async.Unix.Inet_addr.t * int | `Unix of string ] as 'a)
    Socket.Type.t
  -> addr:'a
  -> ([ `Active ], 'a) Socket.t Deferred.t

val tcp_server_generic
  :  ?max_connections:int
  -> addr:'a
  -> stype:'a Socket.Type.t
  -> on_client_error:[`Ignore | `Call of ('a -> exn -> unit)]
  -> f:('a -> Reader.t -> Writer.t -> unit Deferred.t)
  -> ?buffer_age_limit:[`At_most of Time.Span.t | `Unlimited]
  -> unit
  -> unit Deferred.t

(** [tcp_server_unix ~addr ~port ?on_client_error ~f] makes a tcp server over
a unix domain socket, listening on a path indicated by addr.  When  clients
connect f will be run and is passed the address of the client, and a Reader and
Writer for communicating with the client. When the deferred returned by f is
determined or f throws an exception the client socket and the reader and writer
will be closed.  The Deferred returned by the function as a whole will be
determined when the server is ready to accept incoming connections *)
val tcp_server_unix
  :  ?max_connections:int
  -> addr:Socket.Address.Unix.t
  -> on_client_error:[`Ignore | `Call of (Socket.Address.Unix.t -> exn -> unit)]
  -> f:(Socket.Address.Unix.t -> Reader.t -> Writer.t -> unit Deferred.t)
  -> ?buffer_age_limit:[`At_most of Time.Span.t | `Unlimited]
  -> unit
  -> unit Deferred.t
