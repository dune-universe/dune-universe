open Gstreamer

let () =
  init ();
  Printf.printf "%s\n%!" (version_string ());
  let bin =
    Pipeline.parse_launch
      "audiotestsrc ! audioconvert ! audioresample ! autoaudiosink"
  in
  ignore (Element.set_state bin Element.State_playing);
  Unix.sleep 5;
  ignore (Element.set_state bin Element.State_null);
  Gstreamer.deinit ();
  Gc.full_major ()
