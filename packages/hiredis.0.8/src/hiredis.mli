(** Hiredis is an OCaml wrapper around the [hiredis] C library *)

(** The [Value.t] type is used to encode Value.ts to communicate with Redis *)
module Value = Hiredis_value

type status =
    | OK
    | ERR of string option

(** Create a command from an array of strings *)
val command : string array -> string option

(** Create a command from an array of Value.ts *)
val command_v : Value.t array -> string option

(** Encode Value.t to string *)
val encode_string : Value.t -> string

(** Decode Value.t from string *)
val decode_string : string -> Value.t option

(** Readers are used to decode Redis Value.ts from buffered input *)
module Reader : sig
    type t

    (** Create a new, empty reader *)
    val create : unit -> t

    (** Feed the reader more input data *)
    val feed : t -> string -> status

    (** Attempt to read a reply from the accumulated input data *)
    val get_reply : t -> Value.t option
end

module Client : sig
    type t

    (** Returns the error string associated with a hiredis context *)
    val error_string : t -> string option

    (** Create a new client connection *)
    val connect :
        ?scripts:(string, string) Hashtbl.t ->
        ?auth:string ->
        ?nonblock:bool ->
        ?port:int ->
        string -> t

    (** Create a new client from an existing file descr *)
    val of_fd :
        ?scripts:(string, string) Hashtbl.t ->
        ?close_fd:bool ->
        Unix.file_descr -> t

    (** Get the underlying file descr *)
    val to_fd : t -> Unix.file_descr

    (** Set a client's timeout *)
    val set_timeout : t -> int -> int -> status

    (** Enable keepalive on the client *)
    val enable_keepalive : t -> status

    (** Queue command to be executed *)
    val append_command : t -> string array -> status

    (** Similar to [append_command] but using a command made of Hiredis Value.ts *)
    val append_command_v : t -> Value.t array -> status

    (** Append a pre-formatted command string to be executed *)
    val append_formatted : t -> string -> status
    val append_value : t -> Value.t -> status

    (* Write queued commands *)
    val flush_buffer : t -> status
    val read_buffer : t -> status

    (** [get_reply client] executes the queued commands and returns the result *)
    val get_reply : t -> Value.t option

    (** Execute a command formatted as an array of strings and return the reply immediately *)
    val run : t -> string array -> Value.t

    (** Execute a command formatted as an array of Value.ts and return the reply immediately *)
    val run_v : t -> Value.t array -> Value.t

    (** [load_script name script] will load a lua script onto the server and make it available from the existing
     *  client as [name]*)
    val load_script : t -> string -> string -> unit

    (** [call_script client name nkeys args] calls a script by name with the given number of keys and arguments *)
    val call_script : t -> string -> int -> string list -> Value.t

    (** Similar to [call_script] but with a list of Value.ts for arguments *)
    val call_script_v : t -> string -> int -> Value.t list -> Value.t
end

(** A pool is used to access one server using many clients *)
module Pool : sig
    type t = Client.t Lwt_pool.t

    (** [create ~port host n] creates a new pool of [n] clients listening on [host:port] *)
    val create : ?port:int -> string -> int -> t

    (** [use pool] returns a new client from the pool *)
    val use : t -> (Client.t -> 'a Lwt.t) -> 'a Lwt.t
end

module Shell : sig
    module Server : sig
        type t

        (** [start ~temp_dir ~config port] starts a new [redis-server] instance on [port] with the given [config].
         *  The config file will be saved to [temp_dir] *)
        val start : ?temp_dir:string -> ?config:(string * string list) list -> int -> t

        (** Stop an active server *)
        val stop : t -> unit
    end

    module Client : sig
        (** Open the interactive client on the given host and port *)
        val interactive : ?host:string -> int -> unit
    end
end
