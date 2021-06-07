let kick ~sample_rate ~gain =
  let adsr = Mm.Audio.Mono.Effect.ADSR.make sample_rate (0.001, 0.1, 0., 1.) in
  let sine =
    new Mm.Audio.Mono.Generator.sine ~volume:(0.4 *. gain) sample_rate 60. in
  let kick = new Mm.Audio.Mono.Generator.adsr adsr sine in
  new Mm.Audio.Generator.of_mono kick

let snare ~sample_rate ~gain =
  let lpf =
    new Mm.Audio.Mono.Effect.biquad_filter sample_rate `Low_pass 2000. 2. in
  let adsr = Mm.Audio.Mono.Effect.ADSR.make sample_rate (0., 0.08, 0., 1.) in
  let noise =
    new Mm.Audio.Mono.Generator.white_noise ~volume:(0.3 *. gain) sample_rate in
  let filtered = new Mm.Audio.Mono.Generator.chain noise lpf in
  let snare = new Mm.Audio.Mono.Generator.adsr adsr filtered in
  new Mm.Audio.Generator.of_mono snare

let hihat ~sample_rate ~gain =
  let hpf =
    new Mm.Audio.Mono.Effect.biquad_filter sample_rate `High_pass 8000. 2. in
  let adsr = Mm.Audio.Mono.Effect.ADSR.make sample_rate (0., 0.05, 0., 1.) in
  let noise =
    new Mm.Audio.Mono.Generator.white_noise ~volume:(0.3 *. gain) sample_rate in
  let filtered = new Mm.Audio.Mono.Generator.chain noise hpf in
  let snare = new Mm.Audio.Mono.Generator.adsr adsr filtered in
  new Mm.Audio.Generator.of_mono snare
