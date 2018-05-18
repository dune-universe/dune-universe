open Nonstd

module Midi_event = struct
  (** [None] is [0] in while outputing but “any” while matching  *)
  type t = Scene_format_t.midi_event = {
    port: int;
    status: int;
    channel: int;
    data1: int;
    data2: int option; (** [None] is [0] in while outputing but “any” while matching  *)
  } 
  let make ~port ~status ~channel ~data1 ?data2 () =
    { port; status; channel; data1; data2; }
  let to_string { port; status; channel; data1; data2; } =
    sprintf "{midi: %d|%x %d - %d%s}" port status channel data1
      (Option.value_map ~default:"" data2 ~f:(sprintf ", %d"))
end

module Id = struct
  type t = Scene_format_t.id
  let glob = ref 0
  let fresh () =
    incr glob;
    Printf.sprintf "id_%d_%d" !glob (Random.int 1_000_000)

  let compare = String.compare
end

module Event = struct
  type t = Scene_format_t.event =
    | Track_ends of Id.t (* For now “ends” means “reaches its end and is about to loop” *)
    | Track_starts of Id.t
    | Midi_input of Midi_event.t
end
module Action = struct
  type event_handler = Scene_format_t.event_handler = {
    name: string;
    events: Event.t list;
    actions: t list;
  }
  and t = Scene_format_t.action =
    | Raw_midi of Midi_event.t 
    | Track_on of (Id.t * int) (* Track-id × Offset *)
    | Track_off of Id.t
    | Bpm_operation of [ `Set of int | `Incr of int | `Decr of int | `Mul of float ]
    | Add_event_handler of event_handler
    | Remove_event_handler of event_handler
    | Remove_event_handler_by_event of Event.t
    | All_tracks_off
    | Stop

  let handler name ~events ~actions = {name; events; actions}
  let add_handler name ~events ~actions =
    Add_event_handler {name; events; actions}
  let remove_handler name ~events ~actions =
    Remove_event_handler {name; events; actions}
end

module Ticked_action = struct
  type t = Scene_format_t.ticked_action = {
    tick: int;
    action: Action.t;
  }
  let make ~tick ~action = {tick; action}
  let compare (a : t) (b : t) = compare a.tick b.tick
end

module Track = struct
  type t = Scene_format_t.track = {
    id: Id.t;
    events: Ticked_action.t list;
    length: int;
    name: string; (** Display name *)
  }
  let make ?(events = []) ~id ~length name = {name; events; id; length}
  let fresh ?events ~length name =
    make name ?events ~length ~id:Id.(fresh ())

  let id t = t.id
end

type t = Scene_format_t.scene = {
  active: Id.t list;
  handlers: Action.event_handler list;
  bpm: int;
  ppqn: int; (** Pulses Per Quarter Note. *)
  tracks: Track.t list [@main];

}

let make ?(active = []) ?(handlers = []) ?(bpm = 120) ?(ppqn = 12 * 4) tracks =
  {active; handlers; bpm; ppqn; tracks}
let empty = make []

let to_string {active; handlers; bpm; ppqn; tracks} =
  sprintf "(scene %dbpm (ppqn %d) %d tracks, %d active, %d handlers)"
    bpm ppqn (List.length tracks) (List.length active) (List.length handlers)

let empty = make []

module Json = struct
  let of_string = Scene_format_j.scene_of_string
  let to_string = Scene_format_j.string_of_scene ?len:None

  let to_channel scene ~channel =
    let obuf = Bi_outbuf.create_channel_writer channel in
    Scene_format_j.write_scene obuf scene;
    Bi_outbuf.flush_channel_writer obuf;
    ()

  let of_channel_exn i =
    let yolex = Yojson.Safe.init_lexer () in
    let lexbuf = Lexing.from_channel i in
    Scene_format_j.read_scene yolex lexbuf

end

module Biniou = struct
  let of_string = Scene_format_b.scene_of_string
  let to_string = Scene_format_b.string_of_scene ?len:None

  let to_channel scene ~channel =
    let obuf = Bi_outbuf.create_channel_writer channel in
    Scene_format_b.write_scene obuf scene;
    Bi_outbuf.flush_channel_writer obuf;
    ()

  let of_channel_exn i =
    (* let yolex = Yojson.Safe.init_lexer () in *)
    let lexbuf = Bi_inbuf.from_channel i in
    Scene_format_b.read_scene (* yolex *) lexbuf

end