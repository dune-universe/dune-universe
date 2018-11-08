
(* read a mol2 file and output its MOP2D unfolded fingerprint *)

open Printf
open Lbvs_consent

module Cons = Consent
module Fp = Fingerprint
module IntSet = MyIntSet
module L = MyList
module Mol = Molecule
module Ht = BatHashtbl
module StringSet = BatSet.String
module Utls = MyUtils

(* create the MOP2D atom env. to bitstring index table *)
let create_mop2d_index fn =
  let mop2d_envs = Utls.map_on_lines_of_file fn Mop2d_env.of_string in
  let res = Ht.create 11 in
  L.iteri (fun i env ->
      (* eprintf "%s\n" (Mop2d_env.to_string env); *)
      assert(not (Ht.mem res env));
      Ht.add res env i
    ) mop2d_envs;
  Log.info "index size: %d" (Ht.length res);
  res

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let input_fn = ref "" in
  let output_fn = ref "" in
  let index_fn = ref "" in
  let usage_message =
    sprintf "usage:\n%s -idx mop2d.index -i molecules.mol2 -o molecules.mop2d\n"
      Sys.argv.(0) in
  Arg.parse
    ["-i", Arg.Set_string input_fn, "<filename.mol2> input molecules";
     "-o", Arg.Set_string output_fn, "<filename.mop2d> output fingerprints";
     "-idx", Arg.Set_string index_fn,
     "<filename.mop2d_index> index of atom environments"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let env2index = create_mop2d_index !index_fn in
  let unfolded_size = Ht.length env2index in
  Utls.with_in_out_file !input_fn !output_fn (fun input output ->
      let counter = ref 0 in
      try
        while true do
          let m = Mol2.read_one counter input in
          if !counter mod 10000 = 0 then
            eprintf "processed %d\r%!" !counter; (* user feedback *)
          let curr_envs = Mini_mol.mop2d_encode m in
          let set_bit_indexes =
            L.fold_left (fun acc env ->
                let i = Ht.find env2index env in
                IntSet.add i acc
              ) IntSet.empty curr_envs in
          (* Bitv.M.to_string: we want the most frequent atom env bits at the
             left of the bitstring *)
          fprintf output "%s,0.0,%d:%s\n"
            Mini_mol.(m.name) unfolded_size (IntSet.to_string set_bit_indexes)
        done
      with End_of_file ->
        eprintf "read %d from %s\n" !counter !input_fn
    )

let () = main ()
