module type AUTH = sig
  type t

  val check : t -> string array -> bool
end

module type DATA = sig
  type data

  module Client : sig
    type t

    val init : data -> t
  end
end

module type SERVER = sig
  type ic

  type oc

  type server

  include DATA

  val run : server -> (ic * oc -> unit Lwt.t) -> unit Lwt.t
end

module type S = sig
  include SERVER

  module Value : Resp.S with type Reader.ic = ic and type Writer.oc = oc

  module Auth : AUTH

  type client = { data : Client.t; ic : ic; oc : oc }

  type command = data -> client -> string -> int -> unit Lwt.t

  val discard_n : client -> int -> unit Lwt.t

  val finish : client -> nargs:int -> int -> unit Lwt.t

  val ok : client -> unit Lwt.t

  val error : client -> string -> unit Lwt.t

  val invalid_arguments : client -> unit Lwt.t

  val send : client -> Resp.t -> unit Lwt.t

  val recv : client -> Resp.t Lwt.t

  type t

  val create :
    ?auth:Auth.t ->
    ?commands:(string * command) list ->
    ?default:string ->
    server ->
    data ->
    t

  val start : t -> unit Lwt.t
end

module type Server = sig
  module type SERVER = SERVER

  module type AUTH = AUTH

  module type DATA = DATA

  module Auth : sig
    module String : AUTH with type t = string

    module User : AUTH with type t = (string, string) Hashtbl.t
  end

  module type S = S

  module Make
      (Server : SERVER)
      (Auth : AUTH)
      (Value : Resp.S
                 with type Reader.ic = Server.ic
                  and type Writer.oc = Server.oc) :
    S
      with type server = Server.server
       and module Auth = Auth
       and type ic = Server.ic
       and type oc = Server.oc
       and module Value = Value
       and type data = Server.data
       and type Client.t = Server.Client.t
end
