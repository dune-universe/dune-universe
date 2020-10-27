open Ezjs_min
open Ezjs_push

let () =
  request_permission @@ fun permission ->
  js_log permission;
  registration "/sw_example.bc.js" @@ fun reg ->
  update_worker reg
