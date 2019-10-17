(**
This module is built around [Resource_pool]. While a pool of type
[Resource_pool.t] manages a number of resources, here we manage a cluster of
such pools. A typical use case would be a cluster of servers, where for each
server we maintain a number of connections. A user of this module can call [use]
to access one of the connections, which are served in a round-robin fashion.

Whenever the [Resource_invalid] exception is raised during [use] the server is
considered dysfunctional which leads to the following measures:
- All connections to the server are closed.
- The server is marked as suspended and will not be used anymore.
- If the [safe] flage of the [Resource_invalid] exception was true, then the
  function supplied to [f] is reexecuted using a different connection on a
  different server.
- Every [check_delay] seconds a [check_server] will be executed in order to see
  whether the server is useable. In this case the suspended-label is removed
  from the server and it will be used again.
*)

val section : Lwt_log.section

module type CONF = sig
  type connection
  type server
  type serverid
  val serverid_to_string : serverid -> string (* for debugging purposes *)
  val connect : server -> connection Lwt.t
  val close : connection -> unit Lwt.t
  val check_delay : float
  val check_server : serverid -> server -> bool Lwt.t
end

module Make (Conf : CONF) : sig
  type server_status = {
    serverid : Conf.serverid;
    desired : int;
    current : int;
    essential : bool;
    suspended : bool;
    check_server : unit -> bool Lwt.t;
    connections : Conf.connection Resource_pool.t;
  }

  val servers : unit -> Conf.serverid list
  val server_statuses : unit -> server_status list
  val server_exists : Conf.serverid -> bool

  (** [remove] marks a given server as removed from the pool. HOWEVER, a number
      of attempts (corresponding to the number of connections to that server)
      to use that server might still occur. These are NOT connection attempts,
      so this does not come with substantial costs. *)
  val remove : Conf.serverid -> unit

  val non_essential_active_connection_pools :
        unit -> (Conf.serverid * Conf.connection Resource_pool.t) list

  (** Adds a server to the pool, permitting a maximum number [num_conn] of
      concurrent connections to that server. If [connect_immediately] is [true]
      (default: [false] then [num_conn] are immediately opened to the server.
      If the given server exists already no action is taken.
      If [essential] is [true] (default: [true]) then the added server is
      protected from being suspended.
  *)
  val add_one :
    ?essential:bool ->
    ?connect_immediately:bool ->
    num_conn:int -> Conf.serverid -> Conf.server -> unit
  (* Corresponds to multiple sequential applications of [add_one] except for the
     fact that [add_many] will result in a better initial scheduling
     distribution. *)
  val add_many :
    ?essential:bool ->
    ?connect_immediately:bool ->
    num_conn:int -> (Conf.serverid * Conf.server) list -> unit
  (* Add a server for which a connection pool already exists *)
  val add_existing :
    ?essential:bool ->
    ?check_server : (unit -> bool Lwt.t) ->
    num_conn:int -> Conf.serverid -> Conf.connection Resource_pool.t -> unit

  (* If [Resource_invalid] is raised by the supplied function, the connection is
     disposed of (using [close]), and a new attempt to run the function is
     launched to the same server as many times as specified by [usage_attempts]
     (default: 1). Be mindful of side-effects that the the function might have
     caused befaure raising [Resource_invalid]. *)
  val use : ?usage_attempts:int -> (Conf.connection -> 'a Lwt.t) -> 'a Lwt.t
end
