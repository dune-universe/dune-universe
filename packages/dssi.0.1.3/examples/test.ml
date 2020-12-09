let () =
  Dssi.init ();
  let p = Dssi.Plugin.load "/usr/lib/dssi/trivial_synth.so" in
  let d = Dssi.Descriptor.descriptor p 0 in
  Printf.printf "API version: %d\n%!" (Dssi.Descriptor.api_version d);
  let ladspa = Dssi.Descriptor.ladspa d in
  let inst = Ladspa.Descriptor.instantiate ladspa 44100 in
  let p_bank, p_program, p_name = Dssi.Descriptor.get_program d inst 0 in
    Printf.printf "Program %d,%d: %s\n%!" p_bank p_program p_name
