module Client : sig
  module Id : sig
    type t

    val to_int : t -> int

    val of_int : int -> t

    val ( = ) : t -> t -> bool

    val compare : t -> t -> int
  end

  type t

  val id : t -> Id.t

  val send : t -> string -> unit Lwt.t

  val send_multiple : t -> string list -> unit Lwt.t
end

module Server : sig
  type t

  val create : port:int -> t

  val run :
    t ->
    ?on_connect:(Client.t -> unit Lwt.t) ->
    ?on_close:(Client.t -> string -> unit Lwt.t) ->
    ?on_client_error:(Client.t -> exn -> unit Lwt.t) ->
    (Client.t -> string -> unit Lwt.t) ->
    unit Lwt.t

  val clients : t -> Client.t list

  val get : t -> Client.Id.t -> Client.t option

  val broadcast : t -> string -> unit Lwt.t

  val broadcast_to_others : t -> Client.t -> string -> unit Lwt.t

  val close : t -> Client.t -> unit Lwt.t

  val close_all : t -> unit Lwt.t

  val current_connections : t -> int
end
