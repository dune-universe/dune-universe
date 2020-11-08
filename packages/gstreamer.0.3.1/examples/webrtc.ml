(* Adapted from http://blog.nirbheek.in/2018/02/gstreamer-webrtc.html *)

open Gstreamer

let () =
  init ();
  Printf.printf "%s\n%!" (version_string ());
  let bin =
    Pipeline.parse_launch
      "v4l2src ! queue ! vp8enc ! rtpvp8pay ! \
       application/x-rtp,media=video,encoding-name=VP8,payload=96 ! webrtcbin \
       name=sendrecv"
  in
  let _ = Bin.get_by_name bin "sendrecv" in
  ignore (Element.set_state bin Element.State_playing);
  Unix.sleep 5;
  ignore (Element.set_state bin Element.State_null);
  Gstreamer.deinit ();
  Gc.full_major ()
