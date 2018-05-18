(** {!Scene.t} is the (lowest-level) description of the initial state of th sequencer. *)

(**
   This module wraps.{!Scene_format_t} which is the “types” output of [atdgen].
   
   The submodule {!Json} wraps functions from {!Scene_format_j}.
*)

(** {!Midi_event.t} is a description of a MIDI event (in the Jack
    Audio Connection Kit sense).
 *)
module Midi_event : sig
  type t =
    Scene_format_t.midi_event = {
    port : int; (** This is the Jack port number (starts at 0). *)
    status : int;
    channel : int;
    data1 : int;
    data2 : int option; (** [data2] is an option to ba able to match MIDI events. *)
  }
  val make :
    port:int ->
    status:int -> channel:int -> data1:int -> ?data2:int -> unit -> t

  val to_string : t -> string
  (** Get a display-friendly string. *)
end

(** 
   The tracks of the sequencer ({!Track.t} values) are referenced through
   {!Id.t} (strings for now).
 *)
module Id : sig
  type t = Scene_format_t.id
  val compare : t -> t -> int

  val fresh : unit -> string
  (** Get a fresh hopefully unique identifier. *)

end

(** The various kinds of events the sequencer can react to. *)
module Event : sig
  type t =
    Scene_format_t.event =
      Track_ends of Id.t
    | Track_starts of Id.t
    | Midi_input of Midi_event.t
end

(** Both tracks and event handlers perform “actions” ({!Action.t}). *)
module Action : sig
  type event_handler =
    Scene_format_t.event_handler = {
    name : string;
    events : Event.t list;
    actions : t list;
  }
  and t = Scene_format_t.action =
    | Raw_midi of Midi_event.t
    | Track_on of (Id.t * int) (** Start track by id after a given number of “ticks.” *)
    | Track_off of Id.t
    | Bpm_operation of
        [ `Decr of int | `Incr of int | `Mul of float | `Set of int ]
    | Add_event_handler of event_handler
    | Remove_event_handler of event_handler
    | Remove_event_handler_by_event of Event.t (** Remove all handlers for a given event. *)
    | All_tracks_off (** Stop all the tracks but keep the sequencer running. *)
    | Stop (** Stop and quit the whole sequencer application. *)

  val handler :
    string ->
    events:Event.t list ->
    actions:t list -> event_handler
  val add_handler :
    string -> events:Event.t list -> actions:t list -> t
  val remove_handler :
    string -> events:Event.t list -> actions:t list -> t
end

(** Tracks are sets of actions with timestamps (in “ticks”) within the track. *)
module Ticked_action : sig
  type t =
    Scene_format_t.ticked_action = {
    tick : int;
    action : Action.t;
  }
  val make : tick:int -> action:Action.t -> t
  val compare : t -> t -> int
end

(** The main unit of loop in the Scene is the {!Track.t}. *)
module Track : sig
  type t =
    Scene_format_t.track = {
    id : Id.t;
    events : Ticked_action.t list;
    length : int;
    name : string; (** Display name *)
  }

  val make :
    ?events:Ticked_action.t list ->
    id:Id.t -> length:int -> string -> t

  val fresh :
    ?events:Ticked_action.t list ->
    length:int -> string -> t
  (** Create a track like {!make} but {!Id.fresh}. *)

  val id : t -> Id.t
end

type t =
  Scene_format_t.scene = {
  active : Id.t list;
  handlers : Action.event_handler list;
  bpm : int;
  ppqn : int; (** Pulses Per Quarter Note. *)
  tracks : Track.t list;
}
(** The toplevel type of the description of a music scene for the sequencer. *)

val make :
  ?active:Id.t list ->
  ?handlers:Action.event_handler list ->
  ?bpm:int -> ?ppqn:int -> Track.t list -> t

val to_string : t -> string
(** [to_string s] renders a human-readable summary description of the scene. *)

val empty : t

(** The serialization-to-JSON functions (as defined by [atdgen]). *)
module Json : sig
  val of_string : string -> t
  val to_string : t -> string
  val to_channel : t -> channel:out_channel -> unit
  val of_channel_exn : in_channel -> t
end

(** The serialization-to-{{:https://github.com/mjambon/biniou}Biniou}
    functions (as defined by [atdgen]). *)
module Biniou: sig
  val of_string : ?pos:int -> string -> t
  val to_string : t -> string
  val to_channel : t -> channel:out_channel -> unit
  val of_channel_exn : in_channel -> t
end