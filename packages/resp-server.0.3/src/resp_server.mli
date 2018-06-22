(** BACKEND defines the minimum needed interface to create a new RESP server *)
module type BACKEND = sig
  (** The server request context type *)
  type t

  (** The client type *)
  type client

  (** Creates new client context *)
  val new_client: t -> client
end

(** AUTH defines the interface for authenticating a client *)
module type AUTH = sig
  (** Authentication type *)
  type t

  (** Used to determine if the client has passed the valid
      authentication to the server *)
  val check: t -> string array -> bool
end

module Value = Hiredis_value
val read_value: Lwt_io.input_channel -> Value.t Lwt.t
val write_value: Lwt_io.output_channel -> Value.t -> unit Lwt.t

(** Client is a pure OCaml implementation of a minimal Redis client *)
module Client: sig
  type t

  (** Connect to a remote server optionally with TLS. If no port is provided then the string arguments
      is assumed to be a Unix socket address. *)
  val connect:
      ?ctx:Conduit_lwt_unix.ctx ->
      ?tls_config:Conduit_lwt_unix.client_tls_config ->
      ?port:int ->
      string ->
      t Lwt.t

  (** Read a value from the connection *)
  val read: t ->  Value.t Lwt.t

  (** Write a value to the connection *)
  val write: t -> Value.t -> unit Lwt.t

  (** Write a command to the connection and return the the resulting value *)
  val run: t -> string array -> Value.t Lwt.t

  (** Similar to {!run}, but take an array of values instead of an array of strings *)
  val run_v: t -> Value.t array -> Value.t Lwt.t
end

(** SERVER defines the interface for a server *)
module type SERVER = sig
  (** Authentication mode *)
  module Auth: AUTH

  (** Backend mode *)
  module Backend: BACKEND

  (** Respond with OK simple string *)
  val ok: Value.t option Lwt.t

  (** Respond with an error *)
  val error: string -> Value.t option Lwt.t

  (** Response with an invalid arguments error *)
  val invalid_arguments: unit -> Value.t option Lwt.t

  (** Underlying server type, this is typically not available to
      consumers of the API *)
  type t

  (** The signature of a command function *)
  type command =
    Backend.t ->
    Backend.client ->
    string ->
    Value.t array ->
    Value.t option Lwt.t

  (** Create a new server instance.
   - auth sets the authentication
   - default sets the default command handler
   - commands sets the commands for the server
   - host is the hostname of the server (ex 127.0.0.1)
   - tls_config configures ssl for the server - see conduit-lwt-unix for more inforation
   - The server type is also inherited from Conduit, but typically you will use either
     `TCP (`PORT $PORTNUMBER) or `Unix_domain_socket (`File $FILENAME)
   *)
  val create :
    ?auth: Auth.t ->
    ?default: command ->
    ?commands: (string * command) list ->
    ?host: string ->
    ?tls_config: Conduit_lwt_unix.tls_server_key ->
    Conduit_lwt_unix.server ->
    Backend.t ->
    t Lwt.t

  (** Run the server *)
  val start:
    ?backlog: int ->
    ?timeout: int ->
    ?stop: unit Lwt.t ->
    ?on_exn: (exn -> unit) ->
    t ->
    unit Lwt.t
end

(** General authentication modes *)
module Auth: sig
  (** Authentication using a single passphrase *)
  module String: AUTH with type t = string
  (** Authentication using usernames and passwords for multiple users *)
  module User: AUTH with type t = (string, string) Hashtbl.t
end

(** Construct a new SERVER with given authentication mode and backend *)
module Make(A: AUTH)(D: BACKEND): SERVER with module Backend = D and module Auth = A
