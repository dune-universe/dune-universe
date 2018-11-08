open Lbvs_consent
  
let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let input_fn = Sys.argv.(1) in
  let output_fn = Sys.argv.(2) in
  let i = ref 0 in
  MyUtils.with_out_file output_fn (fun output ->
      MyUtils.with_in_file input_fn (fun input ->
          try
            while true do
              let m = Sdf.read_one input in
              let inchikey = Sdf.get_inchikey m in
              incr i;
              Log.info "%s" inchikey;
              Printf.fprintf output "%s" m
            done
          with End_of_file ->
            Log.info "read %d" !i
        )
    )

let () = main ()
