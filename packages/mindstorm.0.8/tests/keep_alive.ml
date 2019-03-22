open Printf

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let () =
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  printf "Current sleep time limit = %!";
  let bat = Mindstorm.NXT.keep_alive conn in
  printf "%i milliseconds\n%!" bat;
  Mindstorm.NXT.close conn
