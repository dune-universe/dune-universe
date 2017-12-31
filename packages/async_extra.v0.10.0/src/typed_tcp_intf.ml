open Core
open Import

module type Binable_t = sig
  type t
  include Binable with type t := t
  include Sexpable with type t := t
end

module type Arg = sig
  module Client_message : sig
    type t
    include Sexpable with type t := t
  end

  module Server_message : sig
    type t
    include Sexpable with type t := t
  end

  module Transport : sig
    type t

    val create : Reader.t -> Writer.t -> t Deferred.t
    val close : t -> unit Deferred.t

    val read : t -> [`Eof | `Ok of Client_message.t] Deferred.t
    val write : t -> Server_message.t -> unit
    val flushed_time : t -> Time.t Deferred.t
  end
end

module type S = sig
  module Client_message : sig type t end
  module Server_message : sig type t end

  module Client_id : Unique_id

  module Server_read_result : sig
    type t =
      | Connect       of Client_id.t
      | Disconnect    of Client_id.t * Sexp.t
      | Denied_access of string
      | Data          of Client_id.t * Client_message.t

    include Sexpable with type t := t
  end

  type t

  val create
    :  ?backlog          : int
    -> ?verbose          : bool (** default is [false] *)
    -> ?log_disconnects  : bool (** default is [true] *)
    -> ?buffer_age_limit : [ `At_most of Time.Span.t | `Unlimited ]
    -> port              : int
    -> auth              : (Unix.Inet_addr.t
                            -> int
                            -> Client_id.t
                            -> [ `Allow | `Deny of string option ] Deferred.t)
    -> unit
    -> t Deferred.t

  (** The server will not accept any connections until you call listen at least once. *)
  val listen : t -> Server_read_result.t Pipe.Reader.t
  val listen_ignore_errors : t -> (Client_id.t * Client_message.t) Pipe.Reader.t
  val close : t -> Client_id.t -> unit

  (** close all the clients and close the server *)
  val close_server : t -> unit Deferred.t
  val flushed_time
    : t -> Client_id.t -> [ `Client_not_found | `Flushed of Time.t Deferred.t ]
  val has_client_id : t -> Client_id.t -> bool
  val send
    : t -> Client_id.t -> Server_message.t -> [`Sent of Time.t | `Drop of exn] Deferred.t
  val send_ignore_errors : t -> Client_id.t -> Server_message.t -> unit
  val send_to_all : t -> Server_message.t -> unit
  val client_addr_port : t -> Client_id.t -> (Unix.Inet_addr.t * int) option
  val port : t -> int
end
