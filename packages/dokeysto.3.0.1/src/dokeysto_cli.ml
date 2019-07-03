
open Printf

module CLI = Minicli.CLI
module Rodb = Dokeysto.Db.RO

let main () =
  Log.set_log_level Log.DEBUG;
  Log.set_output stderr;
  Log.color_on ();
  let _argc, args = CLI.init () in
  (* read all given db *)
  let fn = CLI.get_string ["-db"] args in
  let db = Rodb.open_existing fn in
  Rodb.iter (fun k _v ->
      printf "k:%s\n%!" k
    ) db

let () = main ()
