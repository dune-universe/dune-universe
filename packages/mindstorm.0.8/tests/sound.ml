open Printf

let bt =
  if Array.length Sys.argv < 2 then (
    printf "%s <bluetooth addr>\n" Sys.argv.(0);
    exit 1;
  )
  else Sys.argv.(1)

let soundfile = "Woops.rso"

let () =
  let conn = Mindstorm.NXT.connect_bluetooth bt in
  printf "Connected!\n%!";
  Mindstorm.NXT.Sound.play_tone conn 800 500;
  Unix.sleep 1;
  Mindstorm.NXT.Sound.play_tone conn 200 500;
  printf "Tone played.\n%!";
  Unix.sleep 1;
  Mindstorm.NXT.Sound.play conn soundfile ~loop:true;
  printf "Sound %S playing... %!" soundfile;
  Unix.sleep 3;
  Mindstorm.NXT.Sound.stop conn;
  printf "done.\n%!"

