
(* split a .sdf file into smaller files with given max number of molecules
   in each *)

open Printf
open Lbvs_consent

let get_mol_reader fn =
  if BatString.ends_with fn ".sdf" then
    let fmt = "%s_%010d.sdf" ^^ "" in
    (Sdf.read_one, fmt)
  else if BatString.ends_with fn ".mol2" then
    let fmt = "%s_%010d.mol2" ^^ "" in
    (Mol2.read_one_raw, fmt)
  else
    failwith (sprintf "Sdf_split.get_mol_reader: unsupported file fomat: %s" fn)

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  (* mandatory options *)
  let input_fn = ref "" in
  let pack_size = ref 1 in (* default *)
  let usage_message =
    sprintf "usage:\n%s -i molecules.(sdf|mol2) -n 10000\n" Sys.argv.(0) in
  let argc = Array.length Sys.argv in
  if argc = 1 then
    let () = eprintf "%s" usage_message in
    let _ = exit 1 in
    () (* for typing *)
  else
    Arg.parse
      ["-i", Arg.Set_string input_fn,
       "<filename.(sdf|mol2)> where to read molecules from";
       "-n", Arg.Set_int pack_size,
       "<int> how many molecules per output file (default=1)"]
      (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
      usage_message;
  assert(BatString.ends_with !input_fn ".mol2" ||
         BatString.ends_with !input_fn ".sdf");
  let read_one, naming_fmt = get_mol_reader !input_fn in
  let base_out_fn = (Filename.remove_extension !input_fn) in
  let output = ref stdout in
  let out_fn = ref "" in
  let file_count = ref 0 in
  let mol_count = ref 0 in
  MyUtils.with_in_file !input_fn (fun input ->
      try
        out_fn := sprintf naming_fmt base_out_fn !file_count;
        output := open_out !out_fn;
        while true do
          let m = read_one input in
          incr mol_count;
          fprintf !output "%s" m;
          if !mol_count mod !pack_size = 0 then
            (close_out !output;
             incr file_count;
             out_fn := sprintf naming_fmt base_out_fn !file_count;
             output := open_out !out_fn);
        done
      with End_of_file ->
        (close_out !output;
         (* rm empty file that was just created *)
         if !mol_count mod !pack_size = 0 then Sys.remove !out_fn)
    )

let () = main ()
