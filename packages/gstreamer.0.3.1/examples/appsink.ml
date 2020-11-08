open Gstreamer

let () =
  init ();
  Printf.printf "%s\n\n%!" (version_string ());
  let uri = "http://radiopi.org:8080/hiphop" in
  let read_pipeline =
    Printf.sprintf
      "uridecodebin uri=\"%s\" ! decodebin ! audioconvert ! audioresample ! \
       appsink max-buffers=10 drop=false sync=false name=\"sink\" \
       caps=\"audio/x-raw,format=S16LE,layout=interleaved,channels=2,rate=44100\""
      uri
  in
  Printf.printf "read_pipeline:\n    %s\n\n%!" read_pipeline;
  let read_bin = Pipeline.parse_launch read_pipeline in
  let sink = Bin.get_by_name read_bin "sink" in
  let sink = App_sink.of_element sink in
  let play_pipeline =
    "appsrc name=\"src\" block=true \
     caps=\"audio/x-raw,format=S16LE,layout=interleaved,channels=2,rate=44100\" \
     format=time ! autoaudiosink"
  in
  Printf.printf "play_pipeline:\n    %s\n\n%!" play_pipeline;
  let play_bin = Pipeline.parse_launch play_pipeline in
  let src = Bin.get_by_name play_bin "src" in
  let src = App_src.of_element src in
  ignore (Element.set_state play_bin Element.State_playing);
  ignore (Element.set_state read_bin Element.State_playing);

  Printf.printf "from App_sink.pull_buffer_data to App_src.push_buffer\n%!";
  for _ = 0 to 100 do
    let buf = App_sink.pull_buffer_data sink in
    let buf = Buffer.of_data buf 0 (Bigarray.Array1.dim buf) in
    App_src.push_buffer src buf
  done;

  Printf.printf "from App_sink.pull_buffer_string to App_src.push_buffer\n%!";
  for _ = 0 to 100 do
    let buf = App_sink.pull_buffer_string sink in
    let buf = Buffer.of_string buf 0 (String.length buf) in
    App_src.push_buffer src buf
  done;

  Printf.printf "from App_sink.pull_buffer_data to App_src.push_buffer_data\n%!";
  for _ = 0 to 100 do
    let buf = App_sink.pull_buffer_data sink in
    App_src.push_buffer_data src buf 0 (Bigarray.Array1.dim buf)
  done;

  ignore (Element.set_state read_bin Element.State_null);
  ignore (Element.set_state play_bin Element.State_null);
  Gstreamer.deinit ();
  Gc.full_major ()
