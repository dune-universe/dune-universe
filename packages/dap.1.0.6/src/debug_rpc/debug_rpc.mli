open Debug_protocol

val log_src : Logs.Src.t

type t
(** The type of rpc connection *)

type progress = <
  start : unit -> unit Lwt.t;
  update : int -> int -> unit Lwt.t;
  finish : unit -> unit Lwt.t;
>

val create : in_:Lwt_io.input_channel -> out:Lwt_io.output_channel -> ?next_seq:int -> unit -> t
(** [create ~in_ ~out ?next_seq ()] Create a rpc connection *)

val event : t -> (module EVENT with type Payload.t = 'a) -> 'a React.E.t
(** [event rpc (module The_event)] Get a [The_event.Payload.t React.E.t] for opposite end sent events *)

val send_event : t -> (module EVENT with type Payload.t = 'a) -> 'a -> unit Lwt.t
(** [send_event rpc (module The_event) payload] Send event with [payload] to the opposite end *)

val set_progressive_command_handler : t -> (module COMMAND with type Arguments.t = 'a and type Result.t = 'b) -> ('a -> progress -> 'b Lwt.t) -> unit
(** [set_command_handler rpc (module The_command) f] Set handler [f] for [The_command] *)

val set_command_handler : t -> (module COMMAND with type Arguments.t = 'a and type Result.t = 'b) -> ('a -> 'b Lwt.t) -> unit
(** [set_command_handler rpc (module The_command) f] Set handler [f] for [The_command] *)

val remove_command_handler : t -> (module COMMAND) -> unit
(** [remove_command_handler rpc (module The_command)] Remove handler for [The_command] *)

val exec_command : t -> (module COMMAND with type Arguments.t = 'a and type Result.t = 'b) -> 'a -> 'b Lwt.t
(** [exec_command rpc (module The_command) arg] Execute [The_command] with [arg] on the opposite end.
  @return [res] Returns promise of [The_command.Result.t]. You can use [Lwt.cancel] on it to cancel the request. *)

val start : t -> unit Lwt.t
(** [start rpc] Start rpc dispatch loop. You must call it before interact with rpc. This method will block until input_channel closed. You may call it in a [Lwt.async] block  *)
