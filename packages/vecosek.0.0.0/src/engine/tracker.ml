open Internal_pervasives
open Vecosek_scene


module Error = struct
  type printable =
    | Printable: 'a * ('a -> string) -> printable
  type t = [
    | `Failure of string
    | `Midi_sequencer of string
    | `Time_master of string
    | `Generic of printable
  ]
  let to_string : t -> string =
    function
    | `Failure s -> sprintf "FAILURE: %s" s
    | `Generic (Printable (p, f)) -> sprintf "Error: %s" (f p)
    | `Time_master s -> sprintf "Time_master: %s" s
    | `Midi_sequencer s -> sprintf "Midi_sequencer: %s" s
end
module type IO = sig
  type ('ok, 'error) t
  val return : 'a -> ('a, _) t
  val (>>=) :
    ('ok_a, 'error) t -> ('ok_a -> ('ok_b, 'error) t) -> ('ok_b, 'error) t
end
module type MIDI_SEQUENCER = sig
  type parameters
  type t
  module Of_io (IO : IO) : sig
    val make : parameters -> (t, Error.t) IO.t
    val close : t -> (unit, Error.t) IO.t
    val output_event :
      t -> Scene.Midi_event.t -> (unit, Error.t) IO.t
    val get_input : t -> (Scene.Midi_event.t array, Error.t) IO.t
  end
end
module type TIME_MASTER = sig
  type t
  module Of_io (IO : IO) : sig
    val make : resolution: float -> t
    val run : t ->
      handler: (float -> (bool, Error.t) IO.t) ->
      (unit, Error.t) IO.t
  end
end
module Make
    (IO : IO)
    (Time_master : TIME_MASTER)
    (Midi_sequencer: MIDI_SEQUENCER)
= struct
  module Midi_io = Midi_sequencer.Of_io (IO)
  module Time_io = Time_master.Of_io (IO)
  type t = {
    scene: Scene.t;
    (* sequencer: Midi_sequencer.t [@opaque]; *)
    sequencer_parameters: Midi_sequencer.parameters [@opaque];
    time_resolution: float;
    max_running_time: float;
    debug: out_channel option;
    output_bpm: [ `No | `Vimebac of (int * int) ];
    start_hook: (unit -> unit) option;
    override_ppqn: int option;
  }
  let make ?debug ?(time_resolution = 0.001) ?override_ppqn ~output_bpm
      ~scene ~sequencer_parameters ~max_running_time ?start_hook () =
    {debug; scene; time_resolution; sequencer_parameters;
     max_running_time; output_bpm; start_hook; override_ppqn}


  let debug: out_channel option ref = ref None

  let dbg ?(force_out = false) =
    match !debug with
    | None -> (ifprintf stderr)
    | Some o ->
      (fun fmt ->
         fprintf (if force_out then stdout else o) ("EngineDBG: " ^^ fmt ^^ "\n%!"))

  let warn fmt = ksprintf (eprintf "Engine: WARNING: %s\n%!") fmt

  module State = struct
    module Id_map = struct
      module M = Map.Make(Scene.Id)
      type 'a t = 'a M.t
      let empty = M.empty
      let add map ~key ~value = M.add key value map
      let iter map ~f = M.iter f map
      let fold m ~init ~f =
        M.fold (fun k v b -> f b (k, v)) m init
      let find_exn map ~key = M.find key map
    end

    module Ticked_action_sequence = struct
      type t = Scene.Ticked_action.t array

      let of_list l : t =
        let res = Array.of_list l in
        Array.sort ~cmp:Scene.Ticked_action.compare res;
        res

      let iter = Array.iter

    end



    module Handlers_table = struct
      open Scene
      module S = Set.Make(struct
          type t = Scene.Action.event_handler
          let compare = compare (* polymorphic compare for now *)
        end)
      type t = (Event.t, S.t ref) Hashtbl.t
      let add t ~handler =
        List.iter handler.Scene.Action.events ~f:begin fun event ->
          match Hashtbl.find t event with
          | some -> some := S.add handler !some
          | exception _ ->
            Hashtbl.add t event (ref (S.add handler S.empty))
        end
      let of_list l =
        let t = Hashtbl.create (List.length l) in
        List.iter l ~f:(fun handler -> add t ~handler);
        t
      let remove t ~handler =
        List.iter handler.Scene.Action.events ~f:begin fun event ->
          match Hashtbl.find t event with
          | some -> some := S.remove handler !some
          | exception _ -> ()
        end
      let remove_all_for_event t ~event =
        Hashtbl.remove t event
      let fold_for_event t ~event ~init ~f =
        match Hashtbl.find t event with
        | some -> S.fold (fun p v -> f v p) !some init
        | exception _ -> init
      let all t =
        let l = ref [] in
        let fiter ev set = S.iter (fun x -> l :=  x :: !l) !set in
        Hashtbl.iter fiter t;
        List.dedup !l
    end

    type track = {
      name: string;
      mutable active: int option; (** The starting tick. *)
      (* [active] cannot be a list because we want to be able to
         stop-them by ID for example. *)
      length: int;
      events: Ticked_action_sequence.t;
    }
    let make_track ~length ~name ?active ~events () =
      {name; active; length; events}

    type t = {
      mutable bpm: int;
      mutable ppqn: int;
      tracks: track Id_map.t;
      handlers: Handlers_table.t;
    }
    let make ~bpm ~ppqn ~tracks ~handlers =
      {bpm; ppqn; tracks; handlers}

    let of_scene scene =
      let { Scene. bpm; ppqn; active; handlers; tracks } = scene in
      let tracks =
        let init = Id_map.empty in
        List.fold ~init tracks ~f:begin fun map {Scene.Track. name; id; events; length } ->
          let active = if List.mem ~set:active id then Some 0 else None in
          let events = Ticked_action_sequence.of_list events in
          Id_map.add map ~key:id
            ~value:(make_track ~length ~name ?active ~events ())
        end
      in
      let handlers = Handlers_table.of_list handlers in
      make ~bpm ~ppqn ~tracks ~handlers

    (**
       Returns the current “tick” by counting how many of them should
       have happened since the previous iteration.

       It also returns the readjusted “elapsed” time, that accounts
       for [int_of_float] imprecisions.

       The [pulse_length] is just for debug display.
    *)
    let current_tick t ~current_date ~previous_date ~previous_tick =
      let beat_length = 60. /. float t.bpm in
      let pulse_length = beat_length /. float t.ppqn in
      let tick_offset =
        (current_date -. previous_date) /. pulse_length |> int_of_float in
      let tick = previous_tick + tick_offset in
      (`Tick tick,
       `Elapsed (previous_date +. (float tick_offset *. pulse_length)),
       `Pulse pulse_length)

    let interesting_events_at_tick t ~tick =
      Id_map.fold ~init:[] t.tracks ~f:begin fun prev (id, track) ->
        match track.active with
        | None -> prev
        | Some start_tick ->
          if tick < start_tick (* Not started yet *)
          then prev
          else
            begin
              let local_tick = ((tick - start_tick) mod track.length) in
              let x = ref [] in
              begin if local_tick = 0
                then x := `Event (Scene.Event.Track_starts id) :: !x
              end;
              begin if local_tick = (track.length - 1)
                then x := `Event (Scene.Event.Track_ends id) :: !x
              end;
              Ticked_action_sequence.iter track.events
                ~f:begin fun { Scene.Ticked_action. tick; action } ->
                  if local_tick = tick
                  then x := `Action action :: !x
                  else ()
                end;
              !x :: prev
            end
      end

    let set_track_on_off t ~id on_off =
      match Id_map.find_exn t.tracks ~key:id with
      | track ->
        track.active <- on_off;
      | exception _ ->
        warn "Track %s, (setting active to `%s`): cannot find track"
          id (Option.value_map on_off ~default:"None" ~f:(sprintf "Some %d"))

    let set_all_tracks_off t =
      Id_map.iter t.tracks ~f:(fun _ tr -> tr.active <- None)

    let all_active_tracks t =
      Id_map.fold t.tracks ~init:[] ~f:(fun prev (id, tr) ->
          match tr.active with
          | Some t -> (id, t) :: prev
          | None -> prev)
  end

  let process_action t ~state ~sequencer ~stopped ~tick ~action =
    let open Scene in
    let open State in
    begin match action with
    | Action.Raw_midi event ->
      Midi_io.output_event sequencer event |> ignore;
      ()
    | Action.Track_on (id, ofset) ->
      State.set_track_on_off state ~id (Some (tick + ofset));
    | Action.Track_off id ->
      State.set_track_on_off state ~id None;
    | Action.All_tracks_off ->
      State.set_all_tracks_off state;
    | Action.Bpm_operation op ->
      let open State in
      let guard v = max 1 v in
      begin match op with
      | `Set v  -> state.bpm <- guard @@ v
      | `Incr v -> state.bpm <- guard @@ state.bpm + v
      | `Decr v -> state.bpm <- guard @@ state.bpm - v
      | `Mul f  -> state.bpm <- guard @@ int_of_float (float state.bpm *. f)
      end;
    | Action.Add_event_handler handler ->
      Handlers_table.add state.handlers ~handler;
    | Action.Remove_event_handler handler ->
      Handlers_table.remove state.handlers ~handler;
    | Action.Remove_event_handler_by_event event ->
      Handlers_table.remove_all_for_event state.handlers ~event;
    | Action.Stop ->
      stopped := true
    end

  let start t =
    let open IO in
    debug := t.debug;
    Midi_io.make t.sequencer_parameters
    >>= fun sequencer ->
    let start = Time.now () in
    let state = State.of_scene t.scene in
    Option.iter t.override_ppqn ~f:(fun i -> state.ppqn <- i);
    let last_tick = ref (-1) in
    let previous_date = ref (- 0.001) in
    let stopped = ref false in
    let handler current_date =
      let (`Tick tick, `Elapsed elapsed, `Pulse pulse_length) =
        State.current_tick state
          ~current_date ~previous_date:!previous_date ~previous_tick:!last_tick in
      previous_date := elapsed;
      dbg "Handler %f (prev: %f, elaps: %f, pulse: %f) tick: %d, last_tick: %d; bpm: %d"
        current_date !previous_date elapsed pulse_length
        tick !last_tick state.State.bpm;
      begin match tick - !last_tick with
      | 0 -> return ()
      | n when n < 0 -> dbg "    tick - last_tick < 0"; return ()
      (* assert false *)
      | nb_to_process ->
        let to_process = List.init nb_to_process ~f:(fun i -> !last_tick + i + 1) in
        (* ( *)
        (*   if nb_to_process > 1 then *)
        (*     eprintf "to_process: [%s]\n%!" *)
        (*       (List.map to_process ~f:(sprintf "%d") |> String.concat ~sep:", ") *)
        (* ); *)
        List.fold to_process ~init:(return ()) ~f:begin fun prev tick ->
          prev
          >>= fun () ->
          Midi_io.get_input sequencer
          >>= fun midi_input ->
          Array.fold_left midi_input ~init:(return ()) ~f:begin fun p_m midi_ev ->
            p_m >>= fun () ->
            let open State in
            let event = Scene.Event.Midi_input midi_ev in
            Handlers_table.fold_for_event state.handlers ~event ~init:()
              ~f:begin fun () eh ->
                List.iter eh.Scene.Action.actions ~f:begin fun action ->
                  (* dbg "    action-from-midi-event: %s" (Scene.Action.show action); *)
                  process_action t ~state ~sequencer ~stopped ~tick ~action;
                end
              end;
            let event_data2less =
              Scene.Event.Midi_input Scene.Midi_event.{ midi_ev with data2 = None } in
            Handlers_table.fold_for_event state.handlers
              ~event:event_data2less ~init:()
              ~f:begin fun () eh ->
                List.iter eh.Scene.Action.actions ~f:begin fun action ->
                  (* dbg "    action-from-midi-event-data2less: %s" (Scene.Action.show action); *)
                  process_action t ~state ~sequencer ~stopped ~tick ~action;
                end
              end;
            return ()
          end
          >>= fun () ->
          let events = State.interesting_events_at_tick state ~tick in
          (* assert (List.length events <= 50); (\* -> a debug guard, will be removed *\) *)
          List.iter events ~f:begin fun el -> List.iter el ~f:begin function
            | `Event event ->
              let open State in
              (* dbg "    track-event: %s" (Scene.Event.show event); *)
              Handlers_table.fold_for_event state.handlers ~event ~init:()
                ~f:begin fun () eh ->
                  List.iter eh.Scene.Action.actions ~f:begin fun action ->
                    (* dbg "    action-from-event: %s" (Scene.Action.show action); *)
                    process_action t ~state ~sequencer ~stopped ~tick ~action;
                  end
                end
            | `Action action ->
              (* dbg "    action-from-track: %s" (Scene.Action.show action); *)
              process_action t ~state ~sequencer ~stopped ~tick ~action;
            end
          end;
          return ()
          >>= fun () ->
          begin match t.output_bpm with
          | `Vimebac (port, interval) when tick mod interval = 0 ->
            let action =
              Scene.Action.Raw_midi (
                let data1 = (state.bpm / 127) in
                let data2 = (state.bpm mod 127) in
                Scene.Midi_event.make
                  ~port ~status:0x80 ~channel:3 ~data1 ~data2 ()
              ) in
            process_action t ~state ~sequencer ~stopped ~tick ~action;
            return  ()
          | _ ->
            return  ()
          end
        end
      end
      >>= fun _ ->
      last_tick := tick;
      return (if Time.now () -. start > t.max_running_time
              then false else not !stopped)
    in
    let timer = Time_io.make ~resolution:t.time_resolution in
    Option.value t.start_hook ~default:(fun () -> ()) ();
    Time_io.run timer ~handler

end