module Seconds : sig
  type t

  val of_float : float -> t

  val value : t -> float
end

module Milliseconds : sig
  type t

  val of_int64 : int64 -> t

  val value : t -> int64

  val of_seconds : Seconds.t -> t
end

module Address : sig
  type t = Host of string | HostPort of string * int

  val host : string -> t

  val host_port : string -> int -> t

  val to_string : t -> string

  val compare : t -> t -> int

  val equal : t -> t -> bool
end

module Channel : sig
  type t = Channel of string | ChannelEphemeral of string

  val to_string : t -> string
end

module Topic : sig
  type t = Topic of string | TopicEphemeral of string

  val to_string : t -> string
end

module Producer : sig
  type t

  val create : ?pool_size:int -> Address.t -> (t, string) result

  val publish : t -> Topic.t -> bytes -> (unit, string) result Lwt.t

  val publish_multi : t -> Topic.t -> bytes list -> (unit, string) result Lwt.t
end

module Consumer : sig
  module Config : sig
    type t

    val create :
      ?max_in_flight:int ->
      ?max_attempts:int ->
      ?backoff_multiplier:float ->
      ?error_threshold:int ->
      ?dial_timeout:Seconds.t ->
      ?read_timeout:Seconds.t ->
      ?write_timeout:Seconds.t ->
      ?lookupd_poll_interval:Seconds.t ->
      ?lookupd_poll_jitter:float ->
      ?heartbeat_interval:Seconds.t ->
      ?max_requeue_delay:Seconds.t ->
      ?default_requeue_delay:Seconds.t ->
      ?client_id:string ->
      ?hostname:string ->
      ?user_agent:string ->
      ?output_buffer_size:int ->
      ?output_buffer_timeout:Seconds.t ->
      ?sample_rate:int ->
      unit ->
      (t, string) result
  end

  type t

  val create :
    ?mode:[ `Nsqd | `Lookupd ] ->
    ?config:Config.t ->
    Address.t list ->
    Topic.t ->
    Channel.t ->
    (bytes -> [ `Ok | `Requeue ] Lwt.t) ->
    t

  val run : t -> unit Lwt.t
end
