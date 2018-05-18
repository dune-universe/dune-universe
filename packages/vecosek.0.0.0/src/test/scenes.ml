(**
   These tests are all Vecosek scenes to load, for example:

{v
./scenes-test.byte Ex3 > /tmp/scene.json
./vecosek.byte run --scene /tmp/scene.json
v}

   The tests are for now also used to experiments with
   “scene-building” APIs, hence the redundancy sometimes.

*)

open Nonstd
module String = Sosa.Native_string
open Vecosek_scene
let dbg fmt = ksprintf (eprintf "TSD: %s\n%!") fmt

module Test_scenes = struct
  let all_ref = ref []
  let add name scene = all_ref := (name, scene) :: !all_ref
  let all () = !all_ref |> List.rev
  let find_exn name =
    List.find !all_ref ~f:(fun (n, s) -> String.is_prefix n ~prefix:name)
    |> Option.value_exn ~msg:(sprintf "Can't find example %S" name)
  let names () = List.rev_map !all_ref ~f:fst
end

let () =
  Test_scenes.add "Empty" Scene.empty

let empty_track length =
  Scene.Track.fresh ~length (sprintf "Empty-track-%d" length)

let () =
  Test_scenes.add "Ex1-100" (
    Scene.make [
      empty_track 100;
    ]
  )

module Util_v0 = struct
  let note ~tick ~length ~channel ~pitch ~velocity = Scene.[
      Ticked_action.make ~tick
        ~action:(Action.Raw_midi (
            Midi_event.make ~port:0
              ~status:0x90 ~channel ~data1:pitch ~data2:velocity ()));
      Ticked_action.make ~tick:(tick + length)
        ~action:(Action.Raw_midi (
            Midi_event.make ~port:0
              ~status:0x80 ~channel ~data1:pitch ()));
    ]
end

(**
   A very simple sinle-track scene:

    - 1 tick = 1/100 second, 10 ms
    - 1 note of length 100 ms (10 ticks) every second
    - 9 notes total, loops every 10 seconds, the last 2 seconds are silent
*)
let () =
  let open Scene in
  Test_scenes.add "Ex2-1000-9-notes" (
    let events =
      List.init 9 ~f:(fun i ->
          let tick = i * 100 in
          Util_v0.note ~tick ~length:10 ~channel:1 ~pitch:(64 + i) ~velocity:64)
      |> List.concat
    in
    let name = "Ex2-track-2" in
    let t1 = Track.fresh ~length:1000 name ~events in
    make
      ~bpm:60
      ~ppqn:100
      ~active:[Track.id t1]
      [
        empty_track 1000;
        t1;
      ]
  )


(**

   {ol
     {li [intro_track]: cowbell metronome, runs for 2 4/4 bars}
     {li [four_bars], is a “meta-track” of four 4/4 bars: {ul
         {li [base_drums]: 4/4 drum pattern}
         {li [cymbal_on_one]: crash cymbal on the 1 of the first bar}
         {li [snare_break]: drum “fill” on the 4th bar}
         }
     }
   }

   As [~handlers]: {ul
     {li every time [four_bars] finishes the tempo is decreased by 20 bpm }
     {li an input midi-event can be used to put the tempo back at 176 bpm (Drum'n'bass) }
     {li another input midi-event can be used to stop Vecosek }
   }


*)
let () =
  let ppqn = 100 in
  let tickf f =
    float ppqn *. f |> int_of_float
  in
  let kick position =
    Util_v0.note ~tick:(tickf position) ~length:(tickf 0.5)
      ~channel:9 ~pitch:36 ~velocity:100
  in
  let snare position =
    Util_v0.note ~tick:(tickf position) ~length:(tickf 0.5)
      ~channel:9 ~pitch:40 ~velocity:110
  in
  let hh position =
    Util_v0.note ~tick:(tickf position) ~length:(tickf 0.2)
      ~channel:9 ~pitch:42 ~velocity:64
  in
  let cowbell position =
    Util_v0.note ~tick:(tickf position) ~length:(tickf 0.2)
      ~channel:9 ~pitch:68 ~velocity:120
  in
  let crash_cymbal position =
    Util_v0.note ~tick:(tickf position) ~length:(tickf 0.2)
      ~channel:9 ~pitch:52 ~velocity:120
  in
  let base_drums =
    let events =
      []
      @ List.concat [kick 0.0; snare 1.; kick 2.5; snare 3.;]
      @
      List.concat [hh 0.; hh 0.25; hh 0.5; hh 1.; hh 1.5; hh 1.75;
                   hh 2.; hh 2.25; hh 2.5; hh 2.75; hh 3.; hh 3.5] in
    Scene.Track.fresh ~length:(tickf 4.) "Base Drums" ~events
  in
  let cymbal_on_one =
    let self = Scene.Id.fresh () in
    let events =
      crash_cymbal 0. @ [
        Scene.Ticked_action.make
          ~tick:(tickf 3.)
          ~action:(Scene.Action.Track_off self);
      ]
    in
    Scene.Track.make ~id:self ~length:(tickf 4.) "Cymbal On One" ~events
  in
  let snare_break =
    let self = Scene.Id.fresh () in
    let events =
      begin
        [2.; 2.25; 2.5; 3.25; 3.5] |> List.map ~f:snare |> List.concat
      end @ [
        Scene.Ticked_action.make
          ~tick:(tickf 3.8)
          ~action:(Scene.Action.Track_off self);
      ]
    in
    Scene.Track.make ~id:self ~length:(tickf 4.) "Snare Break" ~events
  in
  let four_bars =
    let self = Scene.Id.fresh () in
    let events =
      [
        Scene.Ticked_action.make
          ~tick:0
          ~action:(Scene.Action.Track_on (Scene.Track.id base_drums, 1));
        Scene.Ticked_action.make
          ~tick:0
          ~action:(Scene.Action.Track_on (Scene.Track.id cymbal_on_one, 1));
        Scene.Ticked_action.make
          ~tick:(tickf 12.0)
          ~action:(Scene.Action.Track_on (Scene.Track.id snare_break, 1));
      ]
    in
    Scene.Track.make ~id:self ~length:(tickf 16.) "4 bars loop" ~events
  in
  let intro_track =
    let self = Scene.Id.fresh () in
    let events =
      (List.init 8 (fun i -> cowbell (float i)) |> List.concat)
      @ [
        Scene.Ticked_action.make
          ~tick:(tickf 7.8)
          ~action:(Scene.Action.Track_on (Scene.Track.id four_bars, tickf 0.2));
        Scene.Ticked_action.make
          ~tick:(tickf 7.8)
          ~action:(Scene.Action.Track_off self);
      ]
    in
    Scene.Track.make ~id:self ~length:(tickf 8.) "Intro Drums" ~events
  in
  let drum_n_bass_tempo = 176 in
  let handlers =
    [
      Scene.Action.handler "End 4 bars → BPM - 20"
        ~events:[Scene.Event.Track_ends (Scene.Track.id four_bars)]
        ~actions:[Scene.Action.Bpm_operation (`Decr 20)];
      Scene.Action.handler "On midi note 36, chan 9 → back to DnB tempo"
        ~events:[
          Scene.Event.Midi_input Scene.Midi_event.{port = 0; status = 0x90;
                                                   channel = 9;
                                                   data1 = 36;
                                                   data2 = None;}]
        ~actions:[Scene.Action.Bpm_operation (`Set drum_n_bass_tempo)];
      Scene.Action.handler "On midi note 37, chan 9 → Stop Vecosek"
        ~events:[
          Scene.Event.Midi_input Scene.Midi_event.{port = 0; status = 0x90;
                                                   channel = 9;
                                                   data1 = 37;
                                                   data2 = None;}]
        ~actions:[Scene.Action.Stop];
    ]
  in
  Test_scenes.add "Ex3-DnB" @@
  Scene.make
    ~bpm:drum_n_bass_tempo
    ~ppqn
    ~active:[Scene.Track.id intro_track]
    ~handlers
    [
      intro_track;
      base_drums;
      cymbal_on_one;
      snare_break;
      four_bars;
    ]


(**

   A very synthetic scene for testing purposes:

   - [drums] are just poom-tchak (for reference while listening)
   - ["track-<n>"] play 5 times a given note over a bar
   - every bar a new ["track-<n+1>"] gets scheduled to start 50 ticks
     later, with the BPM multiplied by 1.2
   - the last track's ending triggers a sequencer stop

 *)
let () =
  let ppqn = 600 in
  let drums =
    let events = [
      Util_v0.note ~tick:0 ~length:(ppqn / 8) ~channel:9 ~pitch:36 ~velocity:100; (* kick *)
      Util_v0.note ~tick:(ppqn) ~length:(ppqn / 8) ~channel:9 ~pitch:40 ~velocity:110;
      Util_v0.note ~tick:(ppqn * 2) ~length:(ppqn / 8) ~channel:9 ~pitch:36 ~velocity:100; (* kick *)
      Util_v0.note ~tick:(ppqn * 3) ~length:(ppqn / 8) ~channel:9 ~pitch:40 ~velocity:110;
    ] |> List.concat in
    Scene.Track.fresh ~length:(ppqn * 4) "Drums" ~events in
  let track n = (* Purely functional track *)
    let id = sprintf "track-%d" n in
    let events = [
      Util_v0.note ~tick:0 ~length:(ppqn / 4) ~channel:1 ~pitch:(64 + n) ~velocity:64;
      Util_v0.note ~tick:(ppqn / 2) ~length:(ppqn / 4) ~channel:1 ~pitch:(64 + n) ~velocity:64;
      Util_v0.note ~tick:(ppqn) ~length:(ppqn) ~channel:1 ~pitch:(64 + n) ~velocity:64;
      Util_v0.note ~tick:(ppqn * 2) ~length:(ppqn / 2) ~channel:1 ~pitch:(64 + n) ~velocity:64;
      Util_v0.note ~tick:(ppqn * 3) ~length:(ppqn / 2) ~channel:1 ~pitch:(64 + n) ~velocity:64;
    ] |> List.concat in
    Scene.Track.make ~id ~length:(ppqn * 4) "Basic" ~events
  in
  let track_nb = 20 in
  let incr_bpm =
    Scene.Action.handler
      (sprintf "Track %s ends → BPM × 120%%" drums.Scene.Track.name)
      ~events:[Scene.Event.Track_ends (drums |> Scene.Track.id)]
      ~actions:[Scene.Action.Bpm_operation (`Mul 1.2)] in
  let stop_after_last =
    let tr = track (track_nb - 1) in
    Scene.Action.handler
      (sprintf "Track %s ends → STOP" tr.Scene.Track.name)
      ~events:[Scene.Event.Track_ends tr.Scene.Track.id]
      ~actions:[Scene.Action.Stop]
  in
  let add_stop_at_the_last =
    let {Scene.Action. name; events; actions} = stop_after_last in
    let tr = track (track_nb - 1) in
    Scene.Action.handler
      (sprintf "%s starts → add handler %s" tr.Scene.Track.name name)
      ~events:[Scene.Event.Track_starts tr.Scene.Track.id]
      ~actions:Scene.Action.[add_handler "Stop at the last" events actions] in
  let activate n =
    let act_next =
      let trn = track n in
      let trsn = track (n + 1) in
      let name =
        sprintf "When %s ends → activate %s"
          trn.Scene.Track.name trsn.Scene.Track.name in
      let events = [Scene.Event.Track_ends (track n |> Scene.Track.id)] in
      let actions = [
        Scene.Action.Track_on (track (n + 1) |> Scene.Track.id, 50)
      ] in
      Scene.Action.handler name ~events ~actions in
    [
      act_next;
      Scene.Action.handler (sprintf "Remove %s" act_next.Scene.Action.name)
        ~events:[Scene.Event.Track_starts (track (n + 1) |> Scene.Track.id)]
        ~actions:[Scene.Action.Remove_event_handler act_next]
    ]
  in
  let handlers =
    add_stop_at_the_last :: incr_bpm ::
    (List.init (track_nb - 1) ~f:activate |> List.concat)
  in
  let tracks = drums :: List.init track_nb ~f:track in
  Test_scenes.add "Ex4-Bigger" @@
  Scene.make
    ~bpm:120
    ~ppqn
    ~active:Scene.[Track.id drums; track 0 |> Track.id]
    ~handlers
    tracks


module Make_scene (Config : sig val bpm: int val ppqn: int end) = struct
  open Config

  type duration = float
  type position = float


  module Internal = struct
    let note_on = 0x90
    let note_off = 0x80
    let note_on_off ~port ~tick ~length ~channel ~pitch ~velocity = Scene.[
        Ticked_action.make ~tick
          ~action:(Action.Raw_midi (
              Midi_event.make ~port
                ~status:note_on ~channel ~data1:pitch ~data2:velocity ()));
        Ticked_action.make ~tick:(tick + length)
          ~action:(Action.Raw_midi (
              Midi_event.make ~port
                ~status:note_off ~channel ~data1:pitch ()));
      ]
    let check name cond =
      if not cond
      then (
        eprintf "Condition failed: %s\n%!" name;
        ksprintf failwith "Condition failed: %s" name
      )

    (** A floatting point number to an amount of ticks *)
    let tick_of_float f =
      check "tickf: ≥ 0" (f >= 0.);
      float ppqn *. f |> int_of_float

    let length_ticks (f : duration) =
      check "length: ≥ 0" (f >= 0.);
      tick_of_float f

    let position_ticks (f : position) =
      check (sprintf "position: %f ≥ 1" f) (f >= 1.);
      tick_of_float (f -. 1.)

  end
  open Internal

  module Tracks = struct
    let drums_channel = 9
    let kick = 36
    let snare = 40
    let hh = 42
    let cowbell = 68
    let crash_cymbal = 52

    type t = {
      name: string;
      id: string;
      length: duration;
      mutable actions: Scene.Ticked_action.t list list;
      mutable active : bool;
    }

    let all : t list ref = ref []

    let track name length f =
      let t =
        { name; id = Scene.Id.fresh (); length; active = false; actions = []} in
      all := t :: !all;
      f t;
      t
    let id t = t.id
    let active t = t.active
    let activate t = t.active <- true
    let add_actions t evs = t.actions <- evs :: t.actions
    let note
        t ~velocity ~channel ~pitch ~(position : position) ~(length : duration) =
      check "note: 1 ≤ pos ≤ trackc-lenth + 1"
        (position >= 1. && position <= t.length +. 1.);
      check "note: length ≥ 0" (length >= 0.);
      check "note: pos + length ≤ track-length + 1"
        (position +. length <= t.length +. 1.);
      add_actions t (
        note_on_off ~velocity ~pitch ~channel
          ~port:0 ~length:(length_ticks length)
          ~tick:(position_ticks position)
      )
    let drum t ?(v = 100) pitch position : unit =
      note t ~velocity:v ~channel:drums_channel
        ~pitch ~position ~length:0.2
    let drums t ?v pitch poses =
      List.iter poses (fun p -> drum t ?v pitch p)
    let track_on t ?(ofset = 1) pos tr =
      add_actions t [
        Scene.Ticked_action.make
          ~tick:(position_ticks pos)
          ~action:(Scene.Action.Track_on (tr.id, ofset));
      ]
    let track_off t pos tr =
      add_actions t [
        Scene.Ticked_action.make
          ~tick:(position_ticks pos)
          ~action:(Scene.Action.Track_off tr.id);
      ]
    let render t =
      let events = List.concat t.actions in
      Scene.Track.make t.name ~length:(length_ticks t.length) ~id:t.id ~events

  end
  module Handlers = struct
    type t = Scene.Action.event_handler
    let initial_ones : t list ref = ref []
    let initial l = initial_ones := !initial_ones @ l
    let on event name = (name, [event])
    let any events name = (name, events)
    let (==>) (name, events) actions = {Scene.Action. name; events; actions}
    let input midi_event = Scene.Event.Midi_input midi_event
    let compile h = h
    let initial_set () =
      List.map !initial_ones compile

    let track_on track = Scene.Action.Track_on (Tracks.id track, 1)
    let track_off track = Scene.Action.Track_off (Tracks.id track)
    let all_tracks_off = Scene.Action.All_tracks_off
    let stop = Scene.Action.Stop
    let ends tr = Scene.Event.Track_ends (Tracks.id tr)
    let add_handler h = Scene.Action.Add_event_handler h
    let remove_all e = Scene.Action.Remove_event_handler_by_event e

    let incr_bpm n = Scene.Action.Bpm_operation (`Incr n)
    let decr_bpm n = Scene.Action.Bpm_operation (`Decr n)

    module VMPK = struct
      (** VMPK On Channel 1, with base octave 3, Transpose 0 *)
      let key key_note = input (
          Scene.Midi_event.make
            ~port:0 ~status:Internal.note_on ~channel:0 ~data1:key_note ()
        )
      let key_z = key 45
      let key_x = key 47
      let key_c = key 48
      let key_v = key 50
    end
  end

  let render () =
    let tracks = List.map !Tracks.all ~f:(fun tr -> Tracks.render tr) in
    let active =
      List.filter_map !Tracks.all
        ~f:(fun tr -> if Tracks.active tr then Some (Tracks.id tr) else None)
    in
    let handlers = Handlers.initial_set () in
    Scene.make ~bpm ~ppqn
      ~active
      ~handlers
      tracks

end
let () =
  let module M = Make_scene (struct let bpm = 120 let ppqn = 192 end) in
  let open M in
  let sound_metronome_44 =
    let open M.Tracks in
    track "Sound-Metronome 4/4" 4. begin fun t ->
      drum t ~v:120 cowbell 1.;
      drum t ~v:100 cowbell 2.;
      drum t ~v:100 cowbell 3.;
      drum t ~v:100 cowbell 4.;
      activate t;
    end
  in
  Tracks.activate sound_metronome_44;
  let _drums_44 =
    let open Tracks in
    track "4/4: Basic Drums Beat" 4. begin fun t ->
      drums t ~v:120 kick [1.; 3.5];
      drums t ~v:100 snare [2.; 4.];
      drums t ~v:80 hh [1.; 1.5; 2.5; 2.75; 3.; 3.25; 3.75; 4.; 4.5];
    end in
  let _cymbal_44 =
    let open Tracks in
    track "4/4: Cymbal on 1" 4. begin fun t ->
      drum t ~v:120 crash_cymbal 1.;
    end in
  let _snare_break_44 =
    let open Tracks in
    track "4/4: Snare break 01" 4. begin fun t ->
      drums t ~v:120 snare [2.75; 3.; 3.5; 4.25; 4.5];
    end in
  let _four_bars_44 =
    let open Tracks in
    track "4/4: Four bars: (cymbal,beat,break)" (4. *. 4.) begin fun t ->
      track_on t 1. _cymbal_44;
      track_on t 1. _drums_44;
      track_off t 3. _cymbal_44;
      track_on t 13. _snare_break_44;
      track_off t 16.9 _snare_break_44;
      track_off t 16.9 _drums_44;
      (* activate t; *)
    end in
  let _drums_128 =
    let open Tracks in
    track "12/8: Basic Drums Beat" 4. begin fun t ->
      drums t ~v:120 kick [1.; 1.66; 2.66; 3.; 3.33; 3.66; 4.66];
      drums t ~v:100 snare [2.; 4.];
      drums t ~v:80 hh [1.; 1.33; 1.66; 2.; 2.33; 3.; 3.33; 3.66; 4.; 4.66];
    end in
  let _snare_break_128 =
    let open Tracks in
    track "12/8: Snare break 01" 4. begin fun t ->
      drums t ~v:120 snare [2.33; 3.; 3.66; 4.66];
    end in
  let _four_bars_128 =
    let open Tracks in
    track "12/8: Four bars: (cymbal,beat,break)" (4. *. 4.) begin fun t ->
      track_on t 1. _cymbal_44;
      track_on t 1. _drums_128;
      track_off t 3. _cymbal_44;
      track_on t 13. _snare_break_128;
      track_off t 16.9 _snare_break_128;
      track_off t 16.9 _drums_128;
      (* activate t; *)
    end in
  let _drums_74 =
    let open Tracks in
    track "7/4: Basic Drums Beat" 7. begin fun t ->
      drums t ~v:120 kick [1.; 3.5; 5.];
      drums t ~v:100 snare [2.; 4.; 6.; 7.];
      drums t ~v:80 hh [1.; 1.5; 2.5; 2.75;
                        3.; 3.25; 3.75; 4.; 4.5;
                        5.; 5.25; 5.5; 6.; 6.5; 7.; 7.5; 7.75];
    end in
  let _four_bars_74 =
    let open Tracks in
    track "7/4: Four bars: (cymbal,beat,break)" (4. *. 7.) begin fun t ->
      track_on t 1. _cymbal_44;
      track_on t 1. _drums_74;
      track_off t 3. _cymbal_44;
      track_off t (1. +. 7. *. 4. -. 0.1) _drums_74;
    end in
  Handlers.(
    initial [
      on VMPK.key_z "Z → start 4/4 at end of bar" ==> [
        add_handler (
          on (ends sound_metronome_44) "Start all 4/4" ==> [
            all_tracks_off;
            track_on _four_bars_44;
            remove_all (ends sound_metronome_44);
            add_handler (
              on VMPK.key_z "Z → start 12/8 at end of 4/4" ==> [
                remove_all VMPK.key_z;
                add_handler (
                  on (ends _four_bars_44) "Start all 12/8" ==> [
                    all_tracks_off;
                    track_on _four_bars_128;
                    remove_all (ends _four_bars_44);
                    remove_all VMPK.key_z;
                    add_handler (
                      on VMPK.key_z "Z → start 7/4 at end of 12/8" ==> [
                        add_handler (
                          on (ends _four_bars_128) "Start all 7/4" ==> [
                            all_tracks_off;
                            track_on _four_bars_74;
                            remove_all VMPK.key_z;
                            remove_all (ends _four_bars_128);
                          ]
                        );
                      ];
                    );
                  ];
                );
              ];
            );
          ];
        );
        remove_all VMPK.key_z;
      ];
      on VMPK.key_x "X → Stop" ==> [
        stop;
      ];
      on VMPK.key_c "C → BPM+=5" ==> [
        incr_bpm 5;
      ];
      on VMPK.key_v "V → BPM-=5" ==> [
        decr_bpm 5;
      ];
    ]
  );
  Test_scenes.add "Ex5-Multi-Drums" @@ render ()


let cmdliner_term () =
  let open Cmdliner in
  let open Term in
  Arg.(
    let doc =
      "Name of the scene to output (if empty: outputs all \
       the names of the scenes)." in
    value & opt (some string) None & info ["scene-name"] ~doc)

let find_and_output scene_name =
  begin match scene_name with
  | None ->
    printf "Available scenes:";
    List.iter (Test_scenes.names ()) ~f:(printf "* %s\n");
    printf "%!";
  | Some sp ->
    let scn = Test_scenes.find_exn sp |> snd in
    eprintf "Scene: %s\n%!" (Scene.to_string scn);
    (* eprintf "Scene: %s\n%!" (Scene.Json.to_string scn); *)
    Scene.Json.to_channel ~channel:stdout scn;
    flush_all ()
  end

let () =
  let open Cmdliner.Term in
  match eval (pure find_and_output $ cmdliner_term (),
              info "scenes-test" ~version:"0")  with
  | `Error _ -> Pervasives.exit 2
  | `Ok ()
  | `Version
  | `Help -> Pervasives.exit 0
