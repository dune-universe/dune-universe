open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events

let trace   = Common.get_by_id "trace"
let name    = Common.get_by_id "name"
let (bus, _) = 
  Broadcast_channel.create_with "say_hello" (Js.string "message")


let callback ev _ = 
  let d = ev##.data in
  let () = Firebug.console ## log (Js.string "Hi") in 
  let _ = Common.text_in name (Js.to_string d)  in
  let _ = Common.write_in trace ("Received from [index.html]: " ^(Js.to_string ev##.data)) in
  Lwt.return_unit

let _ = 
  Lwt_js_events.async_loop
    Broadcast_channel.lwt_js_message
    bus
    callback
