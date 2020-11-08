open Gstreamer

let () =
  Gstreamer.init ();
  if Array.length Sys.argv < 2 then (
    Printf.eprintf "Please provide some files as arguments.\n%!";
    exit 1 );
  let bin = Element_factory.make "playbin" "play" in
  let play fileName =
    Element.set_property_string bin "uri" ("file://" ^ fileName);
    Element.set_state bin Element.State_playing |> ignore;
    Bus.timed_pop_filtered (Bus.of_element bin) [`End_of_stream] |> ignore;
    Element.set_state bin Element.State_null |> ignore
  in
  Array.to_list Sys.argv |> List.tl |> List.iter play;
  Gstreamer.deinit ();
  Gc.full_major ()
