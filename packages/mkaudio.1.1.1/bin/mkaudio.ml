open Cmdliner

(* man page definition. *)
let help man_format cmds topic =
  match topic with
  | None -> `Help (`Pager, None)
  | Some topic ->
    let topics = "topics" :: cmds in
    let conv, _ = Arg.enum (List.rev_map (fun s -> (s, s)) topics) in
    match conv topic with
    | `Error e -> `Error (false, e)
    | `Ok t when t = "topics" ->
      List.iter print_endline topics;
      `Ok (Result.Ok ())
    | `Ok t when List.mem t cmds -> `Help (man_format, Some t)
    | `Ok _ ->
      let page = (topic, 7, "", "", ""), [`S topic; `P "Say something"] in
      Manpage.print man_format Format.std_formatter page;
      `Ok (Result.Ok ())

let help_secs = [
  `S "MORE HELP";
  `P "Use `$(mname) $(i,command) --help' for help on a single command.";
  `Noblank;
]

let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "display help about mkaudio" in
  let man = [
    `S "DESCRIPTION";
    `P "Prints help about mkaudio commands."
  ] @ help_secs in
  Term.(ret (pure help $ Term.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~man

(* Argument definitions. *)
let channels =
  let doc = "The number of channels to use when creating the audio file." in
  Arg.(value & opt int 2 & info ["channels"] ~docv:"CHANNELS" ~doc)

let sample_rate =
  let doc = "The sample rate to use when creating the audio file." in
  Arg.(value & opt int 44100 & info ["sample-rate"] ~docv:"SAMPLERATE" ~doc)

let gain =
  let doc = "The gain to apply to the audio output." in
  Arg.(value & opt float 1.0 & info ["gain"] ~docv:"GAIN" ~doc)

let duration =
  let doc = "The duration of the created file. Expected format is a number
             followed by 's', 'm' or 'h', specifying seconds, minutes or hours
             respectively. For example, 5s is 5 seconds, 2.5m is 2.5 minutes"
  in
  Arg.(value & opt (some string) None & info ["duration"] ~docv:"DURATION" ~doc)

let tempo =
  let doc = "The tempo of the created file in beats per minute." in
  Arg.(value & opt (some float) None & info ["tempo"] ~docv:"TEMPO" ~doc)

let beats =
  let doc = "The duration of the created file in beats." in
  Arg.(value & opt (some int) None & info ["beats"] ~docv:"BEATS" ~doc)

let frequency =
  let doc = "The frequency of the generated sound" in
  Arg.(value & opt float 440. & info ["frequency"] ~docv:"FREQUENCY" ~doc)

let kick =
  let doc = "String describing the kick drum pattern." in
  Arg.(value & opt (some string) None & info ["kick"] ~docv:"KICK" ~doc)

let snare =
  let doc = "String describing the snare drum pattern." in
  Arg.(value & opt (some string) None & info ["snare"] ~docv:"SNARE" ~doc)

let hihat =
  let doc = "String describing the hihat drum pattern." in
  Arg.(value & opt (some string) None & info ["hihat"] ~docv:"HIHAT" ~doc)

let repeats =
  let doc = "Number of times the beat will be repeated" in
  Arg.(value & opt int 1 & info ["repeats"] ~docv:"REPEATS" ~doc)

let output_file =
  let doc = "The file to write." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"OUTPUTFILE" ~doc)

(* Command definitions. *)
let saw_cmd =
  let doc = "write an audio file containing a saw wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a saw wave.";
  ] @ help_secs in
  Term.(pure Commands.saw
    $ channels
    $ sample_rate
    $ gain
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "saw" ~doc ~man

let sine_cmd =
  let doc = "write an audio file containing a sine wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a sine wave.";
  ] @ help_secs in
  Term.(pure Commands.sine
    $ channels
    $ sample_rate
    $ gain
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "sine" ~doc ~man

let square_cmd =
  let doc = "write an audio file containing a square wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a square wave.";
  ] @ help_secs in
  Term.(pure Commands.square
    $ channels
    $ sample_rate
    $ gain
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "square" ~doc ~man

let triangle_cmd =
  let doc = "write an audio file containing a triangle wave." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a triangle wave.";
  ] @ help_secs in
  Term.(pure Commands.triangle
    $ channels
    $ sample_rate
    $ gain
    $ duration
    $ tempo
    $ beats
    $ frequency
    $ output_file),
  Term.info "triangle" ~doc ~man

let white_noise_cmd =
  let doc = "write an audio file containing white noise." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing white noise.";
  ] @ help_secs in
  Term.(pure Commands.white_noise
    $ channels
    $ sample_rate
    $ gain
    $ duration
    $ tempo
    $ beats
    $ output_file),
  Term.info "white-noise" ~doc ~man

let beat_cmd =
  let doc = "write an audio file containing a beat." in
  let man = [
    `S "DESCRIPTION";
    `P "Write an audio file containing a beat.";
    `P "";
    `P "Kick, snare and hihat patterns should be specified as equal-length
        strings, where each character which is '1' or 'x' corresponds to a
        drum hit, and any other character corresponds to a lack of drum hit.";
  ] @ help_secs in
  Term.(pure Commands.beat
    $ channels
    $ sample_rate
    $ gain
    $ tempo
    $ kick
    $ snare
    $ hihat
    $ repeats
    $ output_file),
  Term.info "beat" ~doc ~man

let default_command =
  let doc = "mkaudio" in
  let man = help_secs in
  Term.(ret (pure (fun _ -> `Help (`Pager, None)) $ pure ())),
  Term.info "mkaudio" ~version:"1.1.0" ~doc ~man

let commands = [
  help_cmd;
  saw_cmd;
  sine_cmd;
  square_cmd;
  triangle_cmd;
  white_noise_cmd;
  beat_cmd;
]

let () =
  Printexc.record_backtrace true;
  match Term.eval_choice default_command commands with
  | `Error _ -> exit 1
  | `Ok (Result.Ok ()) | `Version | `Help -> ()
  | `Ok (Result.Error msg) ->
    print_endline msg;
    exit 1
