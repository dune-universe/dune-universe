open Cron

let tempfile =
  Filename.temp_file "ocaml-cron" ""

let hello =
  make_entry (Printf.sprintf "echo 'This is ocaml-cron writing!' > %s" tempfile)

let to_read = "
http://www.cs.yale.edu/homes/perlis-alan/quotes.html
https://fr.wikiquote.org/wiki/Edsger_Dijkstra
https://fr.wikiquote.org/wiki/Donald_Knuth
"

let cleanup () =
  Printf.printf "Cleaning up...\n%!";
  crontab_remove_entry hello;
  ignore (Sys.command (Printf.sprintf "rm %s" tempfile))

let _initialize =
  Sys.(set_signal sigint Signal_ignore);
  crontab_insert_entry hello;
  Pervasives.at_exit cleanup

let _main =
  Printf.printf "crontab is:\n%s\n\n%!" (string_of_crontab (crontab_get ()));
  Printf.printf "Let us wait together for one minute...\n\n%!";
  Printf.printf "You can read one of these in the meantime:\n%s\n%!" to_read;
  Unix.sleep 70;
  Printf.printf "This is %s: (It should not be empty!)\n%!" tempfile;
  ignore (Sys.command (Printf.sprintf "cat %s" tempfile))
