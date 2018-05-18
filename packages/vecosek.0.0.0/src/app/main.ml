open Vecosek_scene
open Vecosek_engine
open Internal_pervasives


module IO = struct
  include Rresult.R
  let return = ok
  let fail = error
end
module Unix_time_master : Tracker.TIME_MASTER =  struct
  type t = {
    resolution : float;
  }
  module Of_io (IO : Tracker.IO) = struct
    open IO
    let make ~resolution = {resolution}
    let run: _ ->
      handler: (float -> (bool, Tracker.Error.t) IO.t) ->
      (unit, Tracker.Error.t) IO.t
      = fun t ~handler ->
        let zero = Unix.gettimeofday () in
        let current = ref 0. in
        let keep_going = ref true in
        while !keep_going do
          ignore begin
            Thread.delay t.resolution;
            current := Unix.gettimeofday () -. zero;
            handler !current
            >>= fun continue ->
            keep_going := continue;
            return ()
          end
        done;
        return ()
  end
end
module Printf_time_master (P: sig
    val outchan: out_channel
  end): Tracker.TIME_MASTER = struct
  type t = {
    resolution : float;
  }
  module Of_io (IO : Tracker.IO) = struct
    open IO
    let make ~resolution = {resolution}
    let run: _ ->
      handler: (float -> (bool, Tracker.Error.t) IO.t) ->
      (unit, Tracker.Error.t) IO.t
      = fun t ~handler ->
        (* let zero = Unix.gettimeofday () in *)
        let current = ref 0. in
        let keep_going = ref true in
        while !keep_going do
          ignore begin
            current := !current +. t.resolution;
            fprintf P.outchan "Printf_time_master: %f → %f\n%!"
              (Unix.gettimeofday ()) !current;
            handler !current
            >>= fun continue ->
            keep_going := continue;
            return ()
          end
        done;
        return ()
  end
end
module Jack_sequencer = struct
  type parameters = string * int
  type t = Misuja.Sequencer.t
  module Of_io (IO : Tracker.IO) = struct
    let make (name, out_ports) =
      let output_ports = Array.init out_ports (sprintf "out%d") in
      IO.return (
        Misuja.Sequencer.make ~name
          ~input_ports:[| "input" |]
          ~output_ports
      )
    let output_event t { Scene.Midi_event.
                         port; status; channel; data1; data2} =
      Misuja.Sequencer.output_event t
        ~port ~stat:status ~chan:channel
        ~dat1:data1 ~dat2:(Option.value ~default:0 data2);
      IO.return ()
    let close t =
      Misuja.Sequencer.close t;
      IO.return ()
    let get_input t =
      Misuja.Sequencer.get_input t
      |> Array.map ~f:(fun (port, status, channel, data1, data2) ->
          Scene.Midi_event.make ~port ~status ~channel ~data1 ~data2 ())
      |> IO.return
  end
end
module Printf_sequencer = struct
  type parameters = out_channel
  type t = out_channel
  let say out fmt =
    ksprintf (fun s -> fprintf out "PSeq: %s\n%!" s) fmt
  module Of_io (IO : Tracker.IO) = struct
    open IO
    let make x = return x
    let output_event out ev =
      say out "Event: %s\n%!" (Scene.Midi_event.to_string ev);
      return ()
    let close outchan =
      say outchan "Closing sequencer";
      return ()
    let get_input outchan =
      let input = [| |] in
      say outchan "Get input %s"
        (String.concat ~sep:", " (Array.to_list input));
      return input
  end
end


module Command_line = struct

  let scene_of_path (scene_path, scene_format) =
    let inchan = open_in scene_path in
    (match scene_format with
    | `Json -> Vecosek_scene.Scene.Json.of_channel_exn
    | `Biniou -> Vecosek_scene.Scene.Biniou.of_channel_exn)
      inchan

  let version = Vecosek_scene.Meta.version

  module Run = struct
    type t = {
      scene_file: string * [`Json | `Biniou ];
      midi_engine: [`Jack | `Printf ];
      log_file: string option;
      debug_file: string option;
      jack_name: string;
      output_ports: int;
      max_running_time: float;
      vimebac_port: int option;
      start_hook_cmd: string option;
      vimebac_interval: int;
      override_ppqn: int option;
      (** Maximmal number of seconds to run for (default is a year). *)

    }
    let cmdliner_term () =
      let open Cmdliner in
      Term.(
        pure (fun scene_path scene_format midi_engine log_file
               debug_file jack_name output_ports max_running_time
               vimebac_port vimebac_interval start_hook_cmd override_ppqn ->
               let scene_file =
                 scene_path,
                 (match scene_format with
                 | Some s -> s
                 | None ->
                   (if Filename.check_suffix scene_path ".json"
                    then `Json else `Biniou)) in
               {scene_file; midi_engine; log_file; start_hook_cmd;
                debug_file; jack_name; output_ports; max_running_time;
                vimebac_port; vimebac_interval; override_ppqn})
        $ Arg.(
            let doc = "The Scene to run on, if the extension is `.json` the \
                       file will parsed as JSON, if not as “Biniou.”" in
            required & opt (some string) None & info ["scene-path"] ~doc)
        $ Arg.(let doc = "The format to use to parse the scene file." in
               value & opt (enum ["json", `Json; "biniou", `Biniou] |> some) None
               & info ["scene-format"] ~doc)
        $ Arg.(let doc = "The MIDI engine to talk to." in
               value & opt (enum ["jack", `Jack; "printf", `Printf]) `Jack
               & info ["midi-engine"] ~doc)
        $ Arg.(let doc = "Log file." in
               value & opt (some string) None & info ["log-file"] ~doc)
        $ Arg.(let doc = "Debug-log file." in
               value & opt (some string) None & info ["debug-file"] ~doc)
        $ Arg.(let doc = "Name of the JACK client." in
               value & opt string "vecosek" & info ["jack-name"] ~doc)
        $ Arg.(let doc = "Number of JACK-MIDI output ports." in
               value & opt int 3 & info ["output-ports"] ~doc)
        $ Arg.(let doc = "Number of seconds to run for before quitting." in
               value & opt float (60. *. 60. *. 24. *. 365.)
               & info ["max-running-time"] ~doc)
        $ Arg.(let doc = "Output the current BPM on port $(docv) every \
                          vimebac-interval ticks \
                          (with vimebac's protocol)." in
               value
               & opt (some int) None
               & info ["vimebac-port"] ~doc)
        $ Arg.(let doc = "If --vimebac-port is present, output the current \
                          BPM eevery $(docv) ticks." in
               value
               & opt int 60
               & info ["vimebac-interval"] ~doc)
        $ Arg.(let doc = "Command to run while starting to output events \
                          (i.e. after the JACK client is setup)." in
               value & opt (some string) None & info ["start-hook"] ~doc)
        $ Arg.(let doc = "Override the PPQN." in
               value
               & opt (some int) None
               & info ["force-ppqn"] ~doc)
      )

    type choice =
      | Modules:
          (module Tracker.TIME_MASTER) *
          (module Tracker.MIDI_SEQUENCER with type parameters = 'a) *
          'a -> choice
    let run { scene_file; midi_engine; log_file; debug_file;
              jack_name; output_ports; start_hook_cmd;
              max_running_time; vimebac_port; vimebac_interval;
              override_ppqn } =
      let scene = scene_of_path scene_file in
      let output_bpm =
        Option.value_map ~default:`No vimebac_port ~f:(fun p ->
            `Vimebac (p, vimebac_interval)) in
      let start_hook =
        Option.map start_hook_cmd ~f:(fun cmd ->
            fun () -> ignore (Thread.create Sys.command cmd)) in
      let tracker_result  =
        let debug = Option.map ~f:open_out debug_file in
        match midi_engine with
        | `Jack ->
          let module T = Tracker.Make (IO) (Unix_time_master) (Jack_sequencer) in
          T.make ?debug ~output_bpm ?start_hook ?override_ppqn
            ~scene ~sequencer_parameters:(jack_name, output_ports)
            ~max_running_time ()
          |> T.start
        | `Printf ->
          let outchan = Option.value_map ~default:stdout ~f:open_out log_file in
          let module PTM = struct let outchan = outchan end in
          let module T =
            Tracker.Make (IO) (Printf_time_master(PTM)) (Printf_sequencer) in
          T.make ?debug ~output_bpm ?start_hook ?override_ppqn
            ~scene ~sequencer_parameters:outchan ~max_running_time ()
          |> T.start
      in
      match  tracker_result with
      | Result.Ok () -> exit 0
      | Result.Error e ->
        eprintf "Time-master error: %S\n%!" (Tracker.Error.to_string e);
        exit 1

    let cmd () =
      let open Cmdliner.Term in
      pure run $ cmdliner_term (), info "run-test" ~version
  end

  module Display = struct
    let test_scenes scene_path =
      let scene = scene_of_path (scene_path, `Json) in
      printf "Scene:\n%s\n\n%!" (Scene.to_string scene)

    let cmd ?(name = "display-test-scenes") () =
      let nnn = name in
      let open Cmdliner in
      let open Term in
      pure test_scenes $
      Arg.(
        let doc = "Scene file-name (only JSON)." in
        required & opt (some string) None & info ["scene-path"] ~doc),
      info nnn ~version

  end
end

let () =
  let open Cmdliner.Term in
  let open Command_line in
  match eval_choice Display.(cmd ~name:"vecosek" ()) [
      Display.(cmd ());
      Run.(cmd ());
    ]
  with
  | `Error _ -> Pervasives.exit 2
  | `Ok ()
  | `Version
  | `Help -> Pervasives.exit 0