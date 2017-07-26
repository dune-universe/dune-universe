open! Core
open! Import

include module type of Versioned_typed_tcp_intf

val protocol_version : [ `Prod | `Test ] ref

module Dont_care_about_mode : Mode

module Make (Z : Arg) :
  S with
  module To_server_msg = Z.To_server_msg and
module To_client_msg = Z.To_client_msg and
module Client_name = Z.Client_name and
module Server_name = Z.Server_name

(** Repeater is used in the cases where we want to inspect and possible alter the flow
    between a client and a server without having to change either the client or the server
    or the protocol between them. It is written with efficiency in mind which dictated some
    design decisions.

    Repeater is created to act on behalf of a particular server. When a client connects to
    the repeater, the repeater in turns makes a connection to the server. From the client
    point of view it looks like it connected to the actual server. Similarly the server
    thinks that it got a connection from the client. Repeater maintains this pair of
    connections internally and transfers messages between them, or issues messages itself,
    all based on application provided callbacks.

    Repeater sends its name using [credentials] field of the Hello message, so it is
    possible for the server or the client to know that the connection is being routed
    through a repeater. That might be useful in certain cases.

    In order to avoid conversions between types, and to be able to just pass the bytes
    through, repeater requires that both [To_server_msg] and [To_client_msg] use a single
    version (i.e. low_version = prod_version = test_version). This restriction is not
    strictly necessary but makes code simpler. It is possible that this will be changed in
    the future *)
module Repeater
    (To_server_msg : Datum)
    (To_client_msg : Datum)
    (Server_name : Name)
    (Client_name : Name)
    (Mode : Mode) :
sig
  type t

  val create
    :  ?is_client_ip_authorized : (string -> bool)
    -> is_client_allowed        : (Client_name.t -> bool)
    -> repeater_name            : string
    -> listen_port              : int  (** repeater will listen on this port *)
    -> server_ip                : string (** the real server's ip, port and name *)
    -> server_port              : int
    -> server_name              : Server_name.t
    -> t Deferred.t

  type ('state, 'send, 'recv) filter
    = 'recv
    -> state       : 'state
    -> client_name : Client_name.t
    -> server_name : Server_name.t
    -> ('send, 'recv) Repeater_hook_result.t

  (** [start t ~on_connect ~to_server_msg_filter ~to_client_msg_filter ~on_error] starts
      listening for connections from clients. For each incoming connection, repeater will
      create a connection to the actual server. The filter callbacks will be used to decide
      how to alter the message flow. Assumption is that majority of callback call will
      result in Pass_on, that is the application will inspect the message, alter its
      internal state and allow the message to go through unaltered. The repeater code is
      optimized for this case.

      It is important for the application to handle [on_error] events, especially
      disconnects. If one connection goes down, the repeater code will not disconnect the
      other side. This allows the application to do any necessary cleanup and possibly
      send messages to the other side. Eventually, application should call
      [close_connection_from_client] which makes sure that both sides are
      disconnected. Until then, a new connection pair between the same client and the
      server cannot be established. Any messages intended for the disconnected side will
      be dropped.

      [on_connect] is called when a client establishes connection to the repeater. If it
      returns an Error, then the connection is terminated. Otherwise, the returned
      connection state will be passed to other callbacks during message processing. This
      can be used by the application to avoid looking up state based on client or server
      names.

      Any exceptions raised by callbacks will stop the receive message loop on that side,
      and will be propagated to the monitor that called [start]. *)
  val start
    :  t
    -> on_connect           : (Client_name.t -> 'state Or_error.t)
    -> to_server_msg_filter : ('state, To_client_msg.t, To_server_msg.t) filter
    -> to_client_msg_filter : ('state, To_server_msg.t, To_client_msg.t) filter
    -> on_error             : (client_name    : Client_name.t
                               -> server_name : Server_name.t
                               -> state       : 'state
                               -> [ `repeater_to_client | `repeater_to_server ]
                               -> Repeater_error.t
                               -> unit)
    -> on_connecting_error  : (client_name    : Client_name.t
                               -> server_name : Server_name.t
                               -> Error.t -> unit)

    -> unit

  val send_to_all_clients : t -> To_client_msg.t -> unit
  val send_from_all_clients : t -> To_server_msg.t -> unit
  val send_to_server_from : t -> Client_name.t -> To_server_msg.t -> unit

  val active_clients : t -> Client_name.t list

  (** Closes both the connection from the client to the repeater and the one from the
      repeater to the server. *)
  val close_connection_from_client : t -> Client_name.t -> unit

  val drop_new_clients   : t -> unit
  val accept_new_clients : t -> unit

  val shutdown : t -> unit Deferred.t
end

(** Helpers to make your types Datumable if they are binable. Works with up
    to 5 versions (easily extensible to more) *)
module Datumable_of_binable : sig
  module type T = sig type t end
  module type T_bin = sig type t [@@deriving bin_io] end

  module V (V : T) (T : T) : sig
    module type S = sig
      val of_v : V.t -> T.t option
      val to_v : T.t -> V.t option
    end
  end

  module Make_datumable5
      (Versions : Versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V4 : T_bin)
      (V5 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
      (V4_cvt : V(V4)(T).S)
      (V5_cvt : V(V5)(T).S)
    : Datumable with type datum = T.t

  module type Pre_versions = sig
    val low_version : Version.t
    val prod_version : Version.t
  end

  module Five_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V4 : T_bin)
      (V5 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
      (V4_cvt : V(V4)(T).S)
      (V5_cvt : V(V5)(T).S)
    : Datumable with type datum = T.t
  ;;

  module Four_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V4 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
      (V4_cvt : V(V4)(T).S)
    : Datumable with type datum = T.t
  ;;

  module Three_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
    : Datumable with type datum = T.t
  ;;

  module Two_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
    : Datumable with type datum = T.t
  ;;

  module One_version
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V1_cvt : V(V1)(T).S)
    : Datumable with type datum = T.t
  ;;
end
