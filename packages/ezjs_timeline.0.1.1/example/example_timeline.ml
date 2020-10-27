open Ezjs_timeline
open Timeline_utils

let () =
  let timeline =
    timeline
      [ slide ~text:(text "test")
          ~media:(media "https://www.google.com")
          (date 2010) ]
  in
  let s =
    Ezjsonm.value_to_string
      (Json_encoding.construct Timeline_encoding.timeline timeline)
  in
  Printf.printf "%s\n%!" s ;
  let t = Timeline.(make "timeline-embed" (SStr s)) in
  Js_of_ocaml.Js.export "timeline" t
