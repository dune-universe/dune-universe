open Js_of_ocaml
module Lwt_js_events = Js_of_ocaml_lwt.Lwt_js_events


let message = Common.get_input "message"
let btn     = Common.get_by_id "send"
let trace   = Common.get_by_id "trace"
let bus     = Broadcast_channel.create "say_hello"

let _ = 
  Lwt_js_events.(
    async_loop
      click
      btn
      (fun _ _ ->
         let value = message##.value in
         let () = Firebug.console ## log (value) in
         let _ = Common.write_in trace ("Send to [receive.html]: " ^(Js.to_string value)) in
         let _ = Broadcast_channel.post bus value in
         Lwt.return_unit
      )
  )
