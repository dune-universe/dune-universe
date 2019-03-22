open Lwt
open Lwt_io
module NXT = Mindstorm_lwt.NXT

let bt =
  if Array.length Sys.argv < 2 then (
    Printf.printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let main() =
  NXT.connect_bluetooth bt >>= fun conn ->
  printf "Current sleep time limit = %!" >>= fun () ->
  NXT.keep_alive conn >>= fun bat ->
  printf "%i milliseconds\n%!" bat >>= fun () ->
  NXT.close conn

let () =
  Lwt_main.run (main ())
