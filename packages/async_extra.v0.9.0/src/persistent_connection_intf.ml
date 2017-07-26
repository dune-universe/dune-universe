(** An actively maintained connection to some service that eagerly and repeatedly attempts
    to reconnect whenever the underlying connection is lost, until a new one can be
    established. *)

open! Core
open! Import


module type Closable = sig
  (** a connection type *)
  type t

  (** [close t] closes the connection.  The returned deferred becomes determined once any
      resources needed to maintain the connection have been released. *)
  val close : t -> unit Deferred.t

  (** [is_closed t] returns true if [close] has ever been called (even if the returned
      deferred has not yet been fulfilled).

      Note that some modules implementing [Closable] may call close internally upon
      noticing that the connection was closed by the other side.  The interface of such a
      module ought to say that this is the case. *)
  val is_closed : t -> bool

  (** [close_finished t] becomes determined at the same time as the result of the first
      call to [close].  [close_finished] differs from [close] in that it does not have the
      side effect of initiating a close. *)
  val close_finished : t -> unit Deferred.t
end

module type S = sig
  type t

  (** The address of a service to which one can connect.  E.g. [Host_and_port.t] is a
      reasonable choice when making a TCP connection.
  *)
  type address

  (** A connection, perhaps embellished with additional information upon connection. *)
  type conn

  module Event : sig
    type t =
      | Attempting_to_connect
      | Obtained_address      of address
      | Failed_to_connect     of Error.t
      | Connected             of conn sexp_opaque
      | Disconnected
    [@@deriving sexp_of]
  end

  (** [create ~server_name ~log ~on_event ~retry_delay get_address] returns a persistent
      connection to a server whose host and port are obtained via [get_address] every
      time we try to connect.  For example, [get_address] might look up a server's host
      and port in catalog at a particular path to which multiple redundant copies of a
      service are publishing their location.  If one copy dies, we get the address of the
      another one when looking up the address afterwards.

      All connection events (see the type above) are passed to the [on_event] callback, if
      given.  If a [~log] is supplied then these events will be written there as well,
      with a "persistent-connection-to" tag value of [server_name], which should be the
      name of the server we are connecting to.

      [`Failed_to_connect error] and [`Obtained_address addr] events are only reported if
      they are distinct from the most recent event of the same type that has taken place
      since the most recent [`Attempting_to_connect] event.

      Connection is retried after [Time.Span.randomize ~percent:0.3 (retry_delay ())]. The
      default for [retry_delay] is [const (sec 10.)]. Note that what this retry delay
      actually throttles is the delay between two connection attempts, so when a
      long-lived connection dies, connection is usually immediately retried, and if that
      failed, wait for another retry delay and retry. *)
  val create
    :  server_name  : string
    -> ?log         : Log.t
    -> ?on_event    : (Event.t -> unit)
    -> ?retry_delay : (unit -> Time.Span.t)
    -> connect      : (address -> conn Or_error.t Deferred.t)
    -> (unit -> address Or_error.t Deferred.t)
    -> t

  (** [connected] returns the first available connection from the time it is called.  When
      currently connected, the returned deferred is already determined.  If [closed] has
      been called, then the returned deferred is never determined. *)
  val connected : t -> conn Deferred.t

  (** The current connection, if any. *)
  val current_connection : t -> conn option

  (** [close t] closes the current connection and stops it from trying to reconnect.
      After the deferred it returns becomes determined, the last connection has been
      closed and no others will be attempted.

      Note: no [close] calls are ever generated internally in response to the connection
      being closed by the other side.
  *)
  include Closable with type t := t
end

module type T = sig
  module Address : sig
    type t [@@deriving sexp_of]
    val equal : t -> t -> bool
  end

  type t
  include Closable with type t := t
end

module type Persistent_connection = sig

  module type S = S
  module type T = T

  module Make (Conn : T) : S
    with type conn    = Conn.t
     and type address = Conn.Address.t

  module Rpc : sig
    include S
      with type conn    = Rpc.Connection.t
       and type address = Host_and_port.t

    (** [create'] is like the [create], but slightly more convenient for constructing
        unembellished rpc connections. *)
    val create'
      :  server_name          : string
      -> ?log                 : Log.t
      -> ?on_event            : (Event.t -> unit)
      -> ?retry_delay         : (unit -> Time.Span.t)
      -> ?via_local_interface : Unix.Inet_addr.t
      -> ?implementations     : _ Rpc.Connection.Client_implementations.t
      -> ?max_message_size    : int
      -> ?make_transport      : Rpc.Connection.transport_maker
      -> ?handshake_timeout   : Time.Span.t
      -> ?heartbeat_config    : Rpc.Connection.Heartbeat_config.t
      -> (unit -> Host_and_port.t Or_error.t Deferred.t)
      -> t
  end

  module Versioned_rpc : S
    with type conn    = Versioned_rpc.Connection_with_menu.t
     and type address = Host_and_port.t
end
