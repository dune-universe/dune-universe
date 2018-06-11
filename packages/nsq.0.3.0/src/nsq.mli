module Address : sig
  type t =
    | Host of string
    | HostPort of string * int

  val host : string -> t
  val host_port : string -> int -> t
  val to_string : t -> string
  val compare : t -> t -> int
  val equal : t -> t -> bool
end

module Channel : sig
  type t =
    | Channel of string
    | ChannelEphemeral of string

  val to_string : t -> string
end

module Topic : sig
  type t =
    | Topic of string
    | TopicEphemeral of string

  val to_string : t -> string
end

type handler_result =
  | HandlerOK
  | HandlerRequeue

module Producer : sig
  type t
  val create : ?pool_size:int -> Address.t -> (t, string) result
  val publish : t -> Topic.t -> bytes -> (unit, string) result Lwt.t
  val publish_multi : t -> Topic.t -> bytes list -> (unit, string) result Lwt.t
end

module Consumer : sig
  type t

  type config

  val create_config :
    ?max_in_flight:int
    -> ?max_attempts:int
    -> ?backoff_multiplier:float
    -> ?dial_timeout:float
    -> ?read_timeout:float
    -> ?write_timeout:float
    -> ?lookupd_poll_interval:float
    -> ?lookupd_poll_jitter:float
    -> ?heartbeat_interval:float
    -> ?max_requeue_delay:float
    -> ?default_requeue_delay:float
    -> ?client_id:string
    -> ?hostname:string
    -> ?user_agent:string
    -> ?output_buffer_size:int
    -> ?output_buffer_timeout:float
    -> ?sample_rate : int
    -> unit
    -> (config, string) result

  type mode =
    | ModeNsqd
    | ModeLookupd

  val create : ?mode:mode -> ?config:config -> Address.t list -> Topic.t -> Channel.t -> (bytes -> handler_result Lwt.t) -> t
  val run : t -> unit Lwt.t
end

