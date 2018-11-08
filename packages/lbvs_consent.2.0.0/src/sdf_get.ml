
(* extract SDF molecules with given names *)

open Printf
open Lbvs_consent

module DB = Dokeysto_camltc.Db_camltc.RW

let db_name_of fn =
  fn ^ ".db"

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  (* mandatory options *)
  let input_fn = ref "" in
  let names = ref "" in
  let usage_message =
    sprintf "usage:\n%s -i molecules.sdf -names \"mol1,mol2,mol10\""
      Sys.argv.(0) in
  let argc = Array.length Sys.argv in
  if argc = 1 then
    (Log.fatal "%s" usage_message;
     exit 1)
  else
    Arg.parse
      ["-i", Arg.Set_string input_fn,
       "<filename> where to read molecules from";
       "-names", Arg.Set_string names,
       "name1[,name2[,...]] which molecules to get"]
      (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
      usage_message;
  (* is there a DB already? *)
  let db_fn = db_name_of !input_fn in
  let db_exists, db =
    if Sys.file_exists db_fn then
      let () = Log.info "creating %s" db_fn in
      (true, DB.open_existing db_fn)
    else
      let () = Log.info "opening %s" db_fn in
      (false, DB.create db_fn) in
  let count = ref 0 in
  if not db_exists then
    MyUtils.with_in_file !input_fn (fun input ->
        try
          while true do
            let m = Sdf.read_one input in
            let name = Sdf.get_fst_line m in
            DB.add db name m;
            incr count;
            if (!count mod 10_000) = 0 then
              eprintf "read %d\r%!" !count;
          done
        with End_of_file ->
          DB.sync db
      );
  let names = BatString.nsplit !names ~by:"," in
  List.iter (fun name ->
      try
        let m = DB.find db name in
        printf "%s" m
      with Not_found ->
        Log.warn "not found: %s" name
    ) names;
  DB.close db

let () = main ()
