open Printf
open Lbvs_consent

(* print out on stdout molecules filtered by unique InChI;
   the InChI must be present in the input SDF file for each molecule *)

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let input_fn = Sys.argv.(1) in
  if not (BatString.ends_with input_fn ".sdf") then
    failwith (sprintf "%s not a .sdf file" input_fn);
  let already_seen = Hashtbl.create 11 in
  let counter = ref 0 in
  MyUtils.with_in_file input_fn (fun input ->
      try
        while true do
          let mol = Sdf.read_one input in
          incr counter;
          let inchi = Sdf.get_inchi mol in
          if not (Hashtbl.mem already_seen inchi) then
            let () = Hashtbl.replace already_seen inchi () in
            printf "%s" mol
        done
      with End_of_file ->
        (Log.info "filter: read %d molecules in %s" !counter input_fn;
         Log.info "filter: unique molecules: %d"
           (Hashtbl.length already_seen));
    )

let () = main ()
