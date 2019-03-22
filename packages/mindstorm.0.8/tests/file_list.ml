open Printf

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let () =
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  printf "Files on the brick:\n%!";
  Mindstorm.NXT.Find.iter conn "*.*" ~f:(fun fname fsize ->
    printf " - %-20S  %-5i bytes\n%!" fname fsize
  )
