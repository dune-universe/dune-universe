(* read a mol2 file and output its MOP2D unfolded fingerprint *)

(* Copyright (C) 2018, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

open Printf

module A = Array
module Fp = Fingerprint
module L = MyList
module Ht = BatHashtbl
module StringSet = BatSet.String
module IntMap = BatMap.Int

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let argc, args = CLI.init () in
  if argc = 1 then
    (eprintf "usage:\n\
              %s [--gtm]\n\
              -i <in.mol2> input molecules\n\
              -o <out.mop2d> output fingerprints\n\
              -idx <in.idx> index of atom environments\n\
              --gtm: output in Todai-GTM format\n"
       Sys.argv.(0);
     exit 1);
  let input_fn = CLI.get_string ["-i"] args in
  let output_fn = CLI.get_string ["-o"] args in
  let index_fn = CLI.get_string ["-idx"] args in
  let todai_gtm_format = CLI.get_set_bool ["--gtm"] args in
  let radius, env2index = Mop2d_env.restore_mop2d_index index_fn in
  let index_size = Ht.length env2index in
  Utls.with_infile_outfile input_fn output_fn (fun input output ->
      if not todai_gtm_format then
        fprintf output "#radius=%d;index=%s\n" radius index_fn;
      let mol_counter = ref 0 in
      let warn_counter = ref 0 in
      try
        while true do
          let m = Mol2.read_one mol_counter input in
          if !mol_counter mod 1000 = 0 then
            eprintf "processed %d\r%!" !mol_counter; (* user feedback *)
          let curr_envs = Mini_mol.mop2d_encode radius m in
          let counted_envs =
            L.fold_left (fun acc env ->
                let i =
                  try Ht.find env2index env
                  with Not_found -> -1 in
                try IntMap.modify i ((+) 1) acc
                with Not_found -> IntMap.add i 1 acc
              ) IntMap.empty curr_envs in
          let env_counts' = IntMap.bindings counted_envs in
          let warnings, env_counts =
            L.partition (fun (env, _count) -> env = -1) env_counts' in
          let string_of_pair (env_i, count) =
            sprintf "%d:%d" env_i count in
          (match warnings with
           | [] -> ()
           | [(_key, count)] -> warn_counter := !warn_counter + count
           | _ -> assert(false)
          );
          let mol_name = Mini_mol.(m.name) in
          if todai_gtm_format then
            begin
              (* Todai's GTM implementation expects a (fixed-length) vector *)
              let buff = Buffer.create 80 in
              Buffer.add_string buff mol_name; (* name *)
              let vector = A.make index_size 0 in
              L.iter (fun (index, count) ->
                  vector.(index) <- count
                ) env_counts;
              A.iter (fun env_count -> (* features *)
                  Printf.bprintf buff "\t%d" env_count
                ) vector;
              Buffer.add_char buff '\n'; (* EOL *)
              fprintf output "%s" (Buffer.contents buff)
            end
          else
            fprintf output "%s,0.0,%s\n"
              mol_name (L.to_string string_of_pair env_counts)
        done
      with End_of_file ->
        eprintf "read %d from %s. warnings: %d\n"
          !mol_counter input_fn !warn_counter
    )

let () = main ()
