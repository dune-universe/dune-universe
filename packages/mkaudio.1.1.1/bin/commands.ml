open Mkaudio_libs
open Fun

let to_steps = function
  | Some beats -> Some (beats * 4)
  | None -> None

let parse_duration_opt = function
  | Some str ->
    Time.parse_duration str >|= (fun duration -> Some duration)
  | None -> Result.Ok None

let get_samples ~sample_rate ~duration ~tempo ~beats =
  parse_duration_opt duration
  >>= fun duration ->
    Time.calculate_samples
      ~sample_rate ~duration ~tempo ~steps:(to_steps beats)

let saw channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.saw ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let sine channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.sine ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let square channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.square ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let triangle channels sample_rate gain duration tempo beats frequency output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.triangle ~volume:gain sample_rate frequency) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let white_noise channels sample_rate gain duration tempo beats output_file =
  get_samples ~sample_rate ~duration ~tempo ~beats
  >>= fun samples ->
    let generator =
      new Audio.Generator.of_mono
        (new Audio.Mono.Generator.white_noise ~volume:gain sample_rate) in
    Wav.write ~channels ~sample_rate ~samples ~generator ~output_file

let kick_gen sample_rate gain =
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0.001, 0.3, 0., 1.) in
  let sine =
    new Audio.Mono.Generator.sine ~volume:(0.4 *. gain) sample_rate 60. in
  let kick = new Audio.Mono.Generator.adsr adsr sine in
  new Audio.Generator.of_mono kick

let snare_gen sample_rate gain =
  let lpf =
    new Audio.Mono.Effect.biquad_filter sample_rate `Low_pass 2000. 2. in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0., 0.08, 0., 1.) in
  let noise =
    new Audio.Mono.Generator.white_noise ~volume:(0.3 *. gain) sample_rate in
  let filtered = new Audio.Mono.Generator.chain noise lpf in
  let snare = new Audio.Mono.Generator.adsr adsr filtered in
  new Audio.Generator.of_mono snare

let hihat_gen sample_rate gain =
  let hpf =
    new Audio.Mono.Effect.biquad_filter sample_rate `High_pass 8000. 2. in
  let adsr = Audio.Mono.Effect.ADSR.make sample_rate (0., 0.05, 0., 1.) in
  let noise =
    new Audio.Mono.Generator.white_noise ~volume:(0.3 *. gain) sample_rate in
  let filtered = new Audio.Mono.Generator.chain noise hpf in
  let snare = new Audio.Mono.Generator.adsr adsr filtered in
  new Audio.Generator.of_mono snare

let beat channels sample_rate gain tempo kick snare hihat repeats output_file =
  let repeats = max 1 repeats in
  Beat.parse_patterns ~kick ~snare ~hihat
  >>= fun steps ->
    Time.calculate_samples
      ~sample_rate ~duration:None ~tempo ~steps:(Some (List.length steps))
  >>= fun samples ->
    let step_length = samples / (List.length steps) in
    let buffer = Audio.create channels step_length in
    let wav = new Audio.IO.Writer.to_wav_file channels sample_rate output_file in
    let rec add_steps buffer offset = function
      | [] -> ()
      | beat :: rest -> begin
        Audio.clear buffer;
        if beat.Beat.kick
        then begin
          let kick_gen' = kick_gen sample_rate gain in
          kick_gen'#fill_add buffer
        end;
        if beat.Beat.snare
        then begin
          let snare_gen' = snare_gen sample_rate gain in
          snare_gen'#fill_add buffer
        end;
        if beat.Beat.hihat
        then begin
          let hihat_gen' = hihat_gen sample_rate gain in
          hihat_gen'#fill_add buffer
        end;
        wav#write buffer;
        add_steps buffer (offset + 1) rest
      end
    in
    for _ = 1 to repeats do
      add_steps buffer 0 steps
    done;
    Result.Ok (wav#close)
