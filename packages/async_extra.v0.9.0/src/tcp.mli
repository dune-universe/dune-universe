(** [Tcp] supports connection to [inet] sockets and [unix] sockets.  These are two
    different types.  We use ['a where_to_connect] to specify a socket to connect to,
    where the ['a] identifies the type of socket. *)

open! Core
open! Import

type 'a where_to_connect constraint 'a = [< Socket.Address.t ]

val to_host_and_port
  :  ?via_local_interface : Unix.Inet_addr.t (** default is chosen by OS *)
  -> ?via_local_port      : int              (** default is chosen by OS *)
  -> string
  -> int
  -> Socket.Address.Inet.t where_to_connect

val to_inet_address
  :  ?via_local_interface : Unix.Inet_addr.t  (** default is chosen by OS *)
  -> ?via_local_port      : int               (** default is chosen by OS *)
  -> Socket.Address.Inet.t
  -> Socket.Address.Inet.t where_to_connect

val to_file          : string                -> Socket.Address.Unix.t where_to_connect
val to_unix_address  : Socket.Address.Unix.t -> Socket.Address.Unix.t where_to_connect

type 'a with_connect_options
  =  ?buffer_age_limit   : [ `At_most of Time.Span.t | `Unlimited ]
  -> ?interrupt          : unit Deferred.t
  -> ?reader_buffer_size : int
  -> ?timeout            : Time.Span.t
  -> 'a


(** [with_connection ~host ~port f] looks up host from a string (using DNS as needed),
    connects, then calls [f], passing the connected socket and a reader and writer for it.
    When the deferred returned by [f] is determined, or any exception is thrown, the
    socket, reader and writer are closed.  The return deferred is fulfilled after f has
    finished processing and the file descriptor for the socket is closed.  If [interrupt]
    is supplied then the connection attempt will be aborted if interrupt is fulfilled
    before the connection has been established.  Similarly, all connection attempts have a
    timeout (default 10s), that can be overridden with [timeout].

    It is fine for [f] to ignore the supplied socket and just use the reader and writer.
    The socket is there to make it convenient to call [Socket] functions. *)
val with_connection
  : ( 'addr where_to_connect
      -> (([ `Active ], 'addr) Socket.t -> Reader.t -> Writer.t -> 'a Deferred.t)
      -> 'a Deferred.t
    ) with_connect_options

(** [connect_sock where_to_connect] creates a socket and opens a TCP connection.  To use
    an existing socket, supply [~socket].  Any errors in the connection will be reported
    to the monitor that was current when [connect_sock] was called. *)
val connect_sock
  :  ?socket    : ([ `Unconnected ], 'addr) Socket.t
  -> ?interrupt : unit Deferred.t
  -> ?timeout   : Time.Span.t
  -> 'addr where_to_connect
  -> ([ `Active ], 'addr) Socket.t Deferred.t


(** [connect ~host ~port] is a convenience wrapper around [connect_sock] that returns the
    socket, and a reader and writer for the socket.  The reader and writer share a file
    descriptor, and so closing one will affect the other by closing its underlying fd.  In
    particular, closing the reader before closing the writer will cause the writer to
    subsequently raise an exception when it attempts to flush internally-buffered bytes to
    the OS, due to a closed fd.  You should close the [Writer] first to avoid this
    problem.

    If possible, use [with_connection], which automatically handles closing.

    It is fine to ignore the returned socket and just use the reader and writer.  The
    socket is there to make it convenient to call [Socket] functions. *)
val connect
  :  ?socket : ([ `Unconnected ], 'addr) Socket.t
  -> ( 'addr where_to_connect
       -> (([ `Active ], 'addr) Socket.t * Reader.t * Writer.t) Deferred.t
     ) with_connect_options

(** A [Where_to_listen] describes the socket that a tcp server should listen on. *)
module Where_to_listen : sig
  type ('address, 'listening_on) t constraint 'address = [< Socket.Address.t ]
    [@@deriving sexp_of]

  type inet = (Socket.Address.Inet.t, int   ) t [@@deriving sexp_of]
  type unix = (Socket.Address.Unix.t, string) t [@@deriving sexp_of]

  val create
    :  socket_type  : 'address Socket.Type.t
    -> address      : 'address
    -> listening_on : ('address -> 'listening_on)
    -> ('address, 'listening_on) t

  val address : ('address, _) t -> 'address
end

module Bind_to_address : sig
  type t =
    | Address of Unix.Inet_addr.t
    | All_addresses
    | Localhost (** equivalent to [Address Unix.Inet_addr.localhost] *)
  [@@deriving sexp_of]
end

module Bind_to_port : sig
  type t =
    | On_port of int
    | On_port_chosen_by_os
  [@@deriving sexp_of]
end

(** Listen on the specified port on the specified addresses *)
val bind_to : Bind_to_address.t -> Bind_to_port.t -> Where_to_listen.inet

(** [on_port port] is [bind_to All_addresses (On_port port)]*)
val on_port              : int ->    Where_to_listen.inet

(** [on_port_chosen_by_os port] is [bind_to All_addresses On_port_chosen_by_os] *)
val on_port_chosen_by_os :           Where_to_listen.inet

(** Listen on a unix domain socket using the specified path *)
val on_file              : string -> Where_to_listen.unix

(** A [Server.t] represents a TCP server listening on a socket. *)
module Server : sig
  type ('address, 'listening_on) t
    constraint 'address = [< Socket.Address.t ]
    [@@deriving sexp_of]

  type inet = (Socket.Address.Inet.t, int)    t [@@deriving sexp_of]
  type unix = (Socket.Address.Unix.t, string) t [@@deriving sexp_of]

  val invariant : (_, _) t -> unit

  val listening_on : (_, 'listening_on) t -> 'listening_on

  val listening_on_address : ('address, _) t -> 'address

  (** [close t] starts closing the listening socket, and returns a deferred that becomes
      determined after [Fd.close_finished fd] on the socket's fd.  It is guaranteed that
      [t]'s client handler will never be called after [close t].  It is ok to call [close]
      multiple times on the same [t]; calls subsequent to the initial call will have no
      effect, but will return the same deferred as the original call.

      With [~close_existing_connections:true], [close] closes the sockets of all existing
      connections.  [close] does not (and cannot) stop the handlers handling the
      connections, but they will of course be unable to write to or read from the socket.
      The result of [close] becomes determined when all the socket file descriptors are
      closed and the socket's fd is closed. *)
  val close
    :  ?close_existing_connections:bool  (** default is [false] *)
    -> (_, _) t
    -> unit Deferred.t

  (** [close_finished] becomes determined after [Fd.close_finished fd] on the socket's fd,
      i.e. the same deferred that [close] returns.  [close_finished] differs from [close]
      in that it does not have the side effect of initiating a close. *)
  val close_finished : (_, _) t -> unit Deferred.t

  (** [is_closed t] returns [true] iff [close t] has been called. *)
  val is_closed : (_, _) t -> bool

  (** Options for server creation:

      [backlog] is the number of clients that can have a connection pending, as with
      {!Unix.listen}.  Additional connections may be rejected, ignored, or enqueued
      anyway, depending on OS, version, and configuration.

      [max_connections] is the maximum number of clients that can be connected
      simultaneously.  The server will not call [accept] unless the number of clients is
      less than [max_connections], although of course potential clients can have
      a connection pending.

      [max_accepts_per_batch] is the maximum number of connections that the server will
      retrieve per blocking {!Unix.accept} call.  Servers that must handle a large number
      of connections tend to observe a stall in connection accept rates when under heavy
      load.  Increasing [max_accepts_per_batch] will ameliorate this effect, increasing
      connection accept rates and overall throughput at the cost of increased contention
      for resources amongst connections.  Servers that are under light load or ones that
      only service small number of connections at a times should see little to no
      difference in behavior for different values of [max_accepts_per_branch].

      [on_handler_error] determines what happens if the handler throws an exception. If an
      exception is raised by on_handler_error (either explicitely via [`Raise], or in the
      closure passed to [`Call]) no further connections will be accepted.

      Supplying [socket] causes the server to use [socket] rather than create a new
      socket.  In this usage, creation does not set [Socket.Opt.reuseaddr] to [true]; if
      you want that, you must set [reuseaddr] before creation. *)
  type ('address, 'listening_on, 'callback) create_options
    =  ?max_connections       : int  (** default is [10_000] *)
    -> ?max_accepts_per_batch : int  (** default is [1] *)
    -> ?backlog               : int  (** default is [10] *)
    -> ?on_handler_error      : [ `Raise  (** default *)
                                | `Ignore
                                | `Call of ('address -> exn -> unit)
                                ]
    -> ?socket                : ([ `Unconnected ], 'address) Socket.t
    -> ('address, 'listening_on) Where_to_listen.t
    -> 'callback
    -> ('address, 'listening_on) t Deferred.t

  (** [create_sock where_to_listen handler] starts a server listening to a socket as
      specified by [where_to_listen].  It returns a server once the socket is ready to
      accept connections.  The server calls [handler address socket] for each client that
      connects.  If the deferred returned by [handler] is ever determined, or [handler]
      raises an exception, then [socket] is closed.

      The server will stop accepting and close the listening socket when an error handler
      raises (either via [`Raise] or [`Call f] where [f] raises), or if [close] is
      called. *)
  val create_sock
    : ('address,
       'listening_on,
       'address -> ([ `Active ], 'address) Socket.t -> unit Deferred.t
      ) create_options

  (** [create where_to_listen handler] is a convenience wrapper around [create_sock] that
      pass a reader and writer for the client socket to the callback.  If the deferred
      returned by [handler] is ever determined, or [handler] raises an exception, then the
      reader and writer are closed.

      [buffer_age_limit] passes on to the underlying writer option of the same name. *)
  val create
    :  ?buffer_age_limit : Writer.buffer_age_limit
    -> ('address,
        'listening_on,
        'address -> Reader.t -> Writer.t -> unit Deferred.t
       ) create_options

  (** [listening_socket t] accesses the listening socket, which should be used with care.
      An anticipated use is with {!Udp.bind_to_interface_exn}.  Accepting connections on
      the socket directly will circumvent [max_connections] and [on_handler_error],
      however, and is not recommended. *)
  val listening_socket : ('address, _) t -> ([ `Passive ], 'address) Socket.t

  val num_connections : (_, _) t -> int

  (** [set_drop_incoming_connections] configures whether each incoming connection will be
      immediately dropped or not.  This is a hack to effectively get a "pause listening"
      feature.  We can't reliably use [backlog] and [max_num_connections] to reject
      incoming connections.  For example, if we reach [max_num_connections], we won't call
      [accept] but OS might still establish TCP connection.  The client will see the
      connection as established but no data will be exchanged and we'd have to rely on TCP
      retransmit timeouts to close the connection.  In many cases we would prefer to
      accept and then immediately close the connection.  This is an intermediate solution
      until we do a more principled solution (but much more complicated) when we close the
      listening socket and then later [bind] and [listen] again when we decide to unpause
      the server.

      [drop_incoming_connections] is set to false.
  *)
  val set_drop_incoming_connections : (_, _) t -> bool -> unit
end
