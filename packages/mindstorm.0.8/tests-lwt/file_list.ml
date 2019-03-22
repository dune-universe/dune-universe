open Lwt
open Lwt_io
module NXT = Mindstorm_lwt.NXT

let bt =
  if Array.length Sys.argv < 2 then (
    Printf.printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let main () =
  NXT.connect_bluetooth bt >>= fun conn ->
  printf "Files on the brick:\n%!" >>= fun () ->
  NXT.Find.iter conn "*.*" ~f:(fun fname fsize ->
    printf " - %-20S  %-5i bytes\n%!" fname fsize
  )

let () =
  Lwt_main.run (main())
