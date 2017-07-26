open Core
open Import

module type Name = sig
  type t [@@deriving sexp]
  include Hashable with type t := t
  include Binable with type t := t
  include Stringable with type t := t
  include Comparable with type t := t
end

module Version : sig
  type t [@@deriving bin_io, sexp]

  val min : t -> t -> t
  val of_int : int -> t
  val to_int : t -> int
  val add : t -> int -> t

  include Hashable with type t := t
end = struct
  include Int
  let add = (+)
end

module type Versions = sig
  val low_version  : Version.t
  val prod_version : Version.t
  val test_version : Version.t
end

type 'a marshal_fun = 'a -> Bigsubstring.t option
type 'a unmarshal_fun = Bigsubstring.t -> 'a option

(** This module describes the type of a given direction of message
    flow. For example it might describe the type of messages from the
    client to the server.  *)
module type Datumable = sig
  type datum
  include Versions

  (** [lookup_marshal_fun v] This function takes a version [v], and returns a
      function that will downgrade (if necessary) the current version to [v] and
      then write it to a bigsubstring.  The contents of these buffers will be copied
      immediatly, so it is safe to reuse the same bigstring for multiple
      marshals. *)
  val lookup_marshal_fun : Version.t -> (datum marshal_fun, exn) Result.t

  (** [lookup_unmarshal_fun v] This function takes a version [v], and returns a
      function that unmarshals a message and upgrades it, returning zero or one
      message as a result of the upgrade. The bigsubstring is only guaranteed
      to contain valid data until the unmarshal function returns, after which it
      may be overwritten immediatly. *)
  val lookup_unmarshal_fun : Version.t -> (datum unmarshal_fun, exn) Result.t
end

module type Datum = sig
  type t
  include Datumable with type datum = t
end

(** This module may be used to implement modes for clients/servers. A
    common scheme is to have two modes, Test, and Production, and to
    want to maintain the invariant that clients in mode Test may not
    talk to servers in mode Production, and that clients in mode
    Production may not talk to servers in mode Test. Versioned
    connection will check that the mode of the client is the same as
    the mode of the server.

    If you don't care about modes, just use Dont_care_about_mode. *)
module type Mode = sig
  type t

  val current : unit -> t
  val (=) : t -> t -> bool

  include Binable with type t := t
  include Sexpable with type t := t
end

module type Arg = sig
  module To_server_msg : Datum
  module To_client_msg : Datum
  module Server_name : Name
  module Client_name : Name
  module Mode : Mode
end

module Read_result = struct
  type ('name, 'data) t =
    { from          : 'name
    ; ip            : string
    ; time_received : Time.t
    ; time_sent     : Time.t
    ; data          : 'data
    }
  [@@deriving bin_io, sexp]
end

(** The messages which the code using this library on the server side needs to process.
    That is, messages received from Clients or otherwise triggered by a Client connection
    behavior. *)
module Server_msg = struct
  module Control = struct
    type 'name t =
      | Unauthorized     of string
      | Duplicate        of 'name
      | Wrong_mode       of 'name
      | Too_many_clients of string
      | Almost_full      of int (** number of free connections *)
      | Connect          of ('name * [`credentials of string])
      | Disconnect       of 'name * Sexp.t
      | Parse_error      of 'name * string
      | Protocol_error   of string
    [@@deriving bin_io, sexp]
  end

  type ('name, 'data) t =
    | Control of 'name Control.t
    | Data of ('name, 'data) Read_result.t
  [@@deriving sexp]
end

(** The messages which the code using this library on the client side needs to process.
    That is, messages received from Server, or caused by a state change of a connection
    to Server. *)
module Client_msg = struct
  module Control = struct
    type 'name t =
      | Connecting
      | Connect        of ('name * [`credentials of string])
      | Disconnect     of 'name * Sexp.t
      | Parse_error    of 'name * string
      | Protocol_error of string
    [@@deriving bin_io, sexp]
  end

  type ('name, 'data) t =
    | Control of 'name Control.t
    | Data    of ('name, 'data) Read_result.t
  [@@deriving bin_io, sexp]
end

module type S = sig
  include Arg

  type 'a logfun =
    [ `Recv of 'recv | `Send of 'send ]
    -> 'remote_name
    -> time_sent_received:Time.t
    -> unit
    constraint 'a = < send : 'send; recv : 'recv; remote_name : 'remote_name >

  module Server : sig
    type t

    include Invariant.S with type t := t


    (** create a new server, and start listening *)
    val create
      :  ?logfun                             : < send : To_client_msg.t;
                                                 recv : To_server_msg.t;
                                                 remote_name : Client_name.t > logfun
      -> ?now:(unit -> Time.t) (** defualt is [Scheduler.cycle_start] *)
      -> ?enforce_unique_remote_name         : bool (** default is [true] *)
      -> ?is_client_ip_authorized            : (string -> bool)
      (** [warn_when_free_connections_lte_pct].  If the number of free connections falls
          below this percentage of max connections an Almost_full event will be generated.
          The default is 5%.  It is required that 0.0 <=
          warn_when_free_connections_lte_pct <= 1.0 *)
      -> ?warn_when_free_connections_lte_pct : float
      -> ?max_clients                        : int (** max connected clients. default 500 *)
      -> listen_port                         : int
      -> Server_name.t
      -> t Deferred.t

    (** get the port that the server is listening on *)
    val port : t -> int

    (** [close t client] close connection to [client] if it
        exists. This does not prevent the same client from connecting
        again later. *)
    val close : t -> Client_name.t -> unit

    (** [listen t] listen to the stream of messages and errors coming from clients *)
    val listen : t -> (Client_name.t, To_server_msg.t) Server_msg.t Stream.t

    (** [listen_ignore_errors t] like listen, but omit error conditions and
        metadata. When listen_ignore_errors is called it installs a filter on
        the stream that never goes away (unless t is destroyed, or you
        provide a [stop]). *)
    val listen_ignore_errors : ?stop:unit Deferred.t -> t -> To_server_msg.t Stream.t

    (** [send t client msg] send [msg] to [client]. @return a
        deferred that will become determined when the message has been
        sent.  In the case of an error, the message will be dropped,
        and the deferred will be filled with [`Dropped] (meaning the
        message was never handed to the OS), otherwise it will be
        filled with with [`Sent tm] where tm is the time (according to
        Time.now) that the message was handed to the operating
        system.  It is possible that the deferred will never become
        determined, for example in the case that the other side hangs,
        but does not drop the connection. *)
    val send
      : t -> Client_name.t -> To_client_msg.t -> [ `Sent of Time.t | `Dropped ] Deferred.t

    (** [send_ignore_errors t client msg] Just like send, but does not report
        results. Your message will probably be sent successfully
        sometime after you call this function. If you receive a
        [Disconnect] error on the listen channel in close time
        proximity to making this call then your message was likely
        dropped. *)
    val send_ignore_errors : t -> Client_name.t -> To_client_msg.t -> unit

    (** [send_to_all t msg] send the same message to all connected clients. *)
    val send_to_all
      :  t
      -> To_client_msg.t
      -> [ `Sent             (** sent successfuly to all clients *)
         | `Dropped          (** not sent successfully to any client *)
         | `Partial_success  (** sent to some clients *)
         ] Deferred.t

    (** [send_to_all_ignore_errors t msg] Just like [send_to_all] but with no error
        reporting. *)
    val send_to_all_ignore_errors : t -> To_client_msg.t -> unit

    (** [send_to_some t msg names] send the same message to multiple connected clients. *)
    val send_to_some
      :  t
      -> To_client_msg.t
      -> Client_name.t list
      -> [ `Sent             (** sent successfuly to all clients *)
         | `Dropped          (** not sent successfully to any client *)
         | `Partial_success  (** sent to some clients *)
         ] Deferred.t

    (** [send_to_some_ignore_errors t msg] Just like [send_to_some] but with no error
        reporting. *)
    val send_to_some_ignore_errors : t -> To_client_msg.t -> Client_name.t list -> unit

    val client_send_version : t -> Client_name.t -> Version.t option

    val client_is_connected : t -> Client_name.t -> bool

    val flushed
      :  t
      -> cutoff:unit Deferred.t
      -> ( [ `Flushed of Client_name.t list ]
           * [ `Not_flushed of Client_name.t list ]
         ) Deferred.t

    val shutdown : t -> unit Deferred.t

    val shutdown_and_disconnect_clients : t -> unit Deferred.t
  end

  module Client : sig
    type t

    (** create a new (initially disconnected) client *)
    val create'
      :  ?logfun              : < recv : To_client_msg.t;
                                  send : To_server_msg.t;
                                  remote_name : Server_name.t> logfun
      -> ?now                 : (unit -> Time.t) (** default is [Scheduler.cycle_start] *)
      -> ?check_remote_name   : bool             (** default is [true] *)
      -> ?credentials         : string
      -> ip                   : string
      -> port                 : int
      -> expected_remote_name : Server_name.t
      -> Client_name.t
      -> t

    (** Just like [create'], but assume empty credentials (for backwards compatibility) *)
    val create
      :  ?logfun              : < recv : To_client_msg.t;
                                  send : To_server_msg.t;
                                  remote_name : Server_name.t> logfun
      -> ?now                 : (unit -> Time.t)  (** default is [Scheduler.cycle_start] *)
      -> ?check_remote_name   : bool              (** default is [true] *)
      -> ip                   : string
      -> port                 : int
      -> expected_remote_name : Server_name.t
      -> Client_name.t
      -> t

    (** [connect t] If the connection is not currently established, initiate one.  Returns
        a deferred that becomes determined when the connection is established. *)
    val connect : t -> unit Deferred.t

    (** If a connection is currently established, close it.  Also, if we're trying to
        connect, give up. *)
    val close_connection : t -> unit

    (** [listen t] @return a stream of messages from the server and errors *)
    val listen : t -> (Server_name.t, To_client_msg.t) Client_msg.t Stream.t

    (** [listen_ignore_errors t] like [listen], but with no errors or meta data.  When
        listen_ignore_errors is called it installs a filter on the stream that never
        goes away (unless t is destroyed or you provide a stop), so you should
        not call it many times throwing away the result.  If you need to do this
        use listen. *)
    val listen_ignore_errors : ?stop:unit Deferred.t -> t -> To_client_msg.t Stream.t

    (** [send t msg] send a message to the server. If the connection is
        not currently established, initiate one.
        @return a deferred that is filled in with either the time the
        message was handed to the OS, or [`Dropped]. [`Dropped] means that
        there was an error, and the message will not be sent. *)
    val send : t -> To_server_msg.t -> [ `Sent of Time.t | `Dropped ] Deferred.t

    (** [send_ignore_errors t] exactly like [send] but with no error reporting. *)
    val send_ignore_errors : t -> To_server_msg.t -> unit

    (** [state t] @return the state of the connection *)
    val state : t -> [ `Disconnected | `Connected | `Connecting ]

    (** [last_connect_error t] returns the error (if any) that happened on the
        last connection attempt. *)
    val last_connect_error : t -> Error.t option

    val flushed : t -> [ `Flushed | `Pending of Time.t Deferred.t ]
  end
end


module Repeater_error = struct
  type t =
    | Parse_error      of string
    | Marshaling_error
    | Disconnect       of Error.t
end

module Repeater_hook_result = struct
  type ('send, 'recv) t =
    | Do_nothing
    | Send_back             of 'send list
    | Send                  of 'recv
    | Pass_on
    | Pass_on_and_send_back of 'send list
end
