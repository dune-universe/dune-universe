open Cron

let _announce =
  Random.self_init ();
  let out = Printf.sprintf "crontab.backup-%d" (Random.bits ()) in
  Printf.printf "Saving the local crontab in %s.\n" out;
  Sys.command (Printf.sprintf "crontab -l > %s" out)

let saved = crontab_get ()

let entry1 = make_entry "true"

let entry2 = make_entry ~minute:(List [single valid_minute 33]) "false"

let table = make [entry1; entry2]

let _put = crontab_install table

let loaded = crontab_get ()

let _restore = crontab_install saved

let check what desc =
  Printf.printf "[%s] %s\n%!" (if what then "OK" else "KO") desc;
  exit (if what then 0 else 1)

let check () =
  check (loaded = table) "Input/Output of crontab"
