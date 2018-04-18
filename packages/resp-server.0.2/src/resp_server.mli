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
   *  authentication to the server *)
  val check: t -> string array -> bool
end

(** SERVER defines the interface for a server *)
module type SERVER = sig
  (** Authentication mode *)
  module Auth: AUTH

  (** Backend mode *)
  module Backend: BACKEND

  (** Respond with OK simple string *)
  val ok: Hiredis.value option Lwt.t

  (** Respond with an error *)
  val error: string -> Hiredis.value option Lwt.t

  (** Response with an invalid arguments error *)
  val invalid_arguments: unit -> Hiredis.value option Lwt.t

  (** Underlying server type, this is typically not available to
   * consumers of the API *)
  type t

  (** The signature of a command function *)
  type command =
    Backend.t ->
    Backend.client ->
    string ->
    Hiredis.value array ->
    Hiredis.value option Lwt.t

  (** Create a new server instance.
   *
   * - auth sets the authentication
   * - default sets the default command handler
   * - commands sets the commands for the server
   * - host is the hostname of the server (ex 127.0.0.1)
   * - tls_config configures ssl for the server - see conduit-lwt-unix for more inforation
   * - The server type is also inherited from Conduit, but typically you will use either
   *   `TCP (`PORT $PORTNUMBER) or `Unix_domain_socket (`File $FILENAME)
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
  val run :
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
