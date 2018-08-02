
open Printf

module Rodb = Dokeysto.Db.RO

let main () =
  Log.set_log_level Log.DEBUG;
  Log.set_output stderr;
  Log.color_on ();
  let argc, args = CLI.init () in
  (* read all given db *)
  let fn = CLI.get_string ["-db"] args in
  let db = Rodb.open_existing fn in
  Rodb.iter (fun k v ->
      printf "k:%s\n%!" k
    ) db

let () = main ()
