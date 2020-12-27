
(* sample pairwise distances between fingerprints *)

open Printf
open Lbvs_consent

module L = MyList

let compare_with_all dist query cands =
  L.map (dist query) cands

let rec study dist acc = function
  | [] -> acc
  | query :: cands ->
    let res = compare_with_all dist query cands in
    study dist (res :: acc) cands

let main () =
  let sample_size = ref 0 in
  let input_fn = ref "" in
  let usage_message =
    sprintf "usage:\n%s -n SAMPLE_SIZE -i molecules.{csv|ecfp4|mop2d}\n"
      Sys.argv.(0) in
  Arg.parse
    ["-i", Arg.Set_string input_fn,
     "<filename> where to read molecules from";
     "-n", Arg.Set_int sample_size,
     "<int> number of molecules to choose randomly"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let molecules = Molecule.from_fp_file !input_fn in
  let fresh_rands = BatRandom.State.make_self_init () in
  let shaken = L.shuffle ~state:fresh_rands molecules in
  let random_sample = L.take !sample_size shaken in
  let scores = study Molecule.tanimoto_score [] random_sample in
  let scores = List.flatten scores in
  let med = L.medianf scores in
  printf "med: %.3f\n" med

let () = main ()
