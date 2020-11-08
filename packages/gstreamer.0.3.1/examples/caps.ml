open Gstreamer

let () =
  Gstreamer.init ();
  let pipeline =
    Printf.sprintf
      "filesrc location=\"%s\" ! typefind name=typefinder ! fakesink"
      Sys.argv.(1)
  in
  let bin = Pipeline.parse_launch pipeline in
  let tf =
    Type_find_element.of_element (Gstreamer.Bin.get_by_name bin "typefinder")
  in
  let cb proba caps =
    Printf.printf "Found caps (%d%%):\n%s\n%!" proba
      (Gstreamer.Caps.to_string caps)
  in
  Type_find_element.on_have_type tf cb;
  ignore (Element.set_state bin Element.State_playing);
  Unix.sleep 1;
  Gc.full_major ()
