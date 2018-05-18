open Containers

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
  val create : ?pool_size:int -> Address.t -> (t, string) Result.t
  val publish : t -> Topic.t -> bytes -> (unit, string) Result.t Lwt.t
  val publish_multi : t -> Topic.t -> bytes list -> (unit, string) Result.t Lwt.t
end

module Consumer : sig
  type t

  type config = {
    max_in_flight : int;
    max_attempts : int;
    backoff_multiplier : float;
  }

  val default_config : unit -> config

  type mode =
    | ModeNsqd
    | ModeLookupd

  val create : ?mode:mode -> ?config:config -> Address.t list -> Topic.t -> Channel.t -> (bytes -> handler_result Lwt.t) -> (t, string) Result.t
  val run : t -> unit Lwt.t
end

