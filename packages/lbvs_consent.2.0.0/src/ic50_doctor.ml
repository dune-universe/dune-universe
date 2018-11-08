
(* output (ic50,weight) pairs for a given set of query molecules *)

open Printf
open Lbvs_consent

module L = MyList

let main () =
  let input_fn = ref "" in
  let usage_message =
    sprintf "usage:\n%s -i molecules.{csv|ecfp4|mop2d}\n" Sys.argv.(0) in
  Arg.parse
    ["-i", Arg.Set_string input_fn, "<filename> where to read molecules from"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let mols = Molecule.from_fp_file !input_fn in
  let pairs, _ht = Molecule.potency_scale mols in
  L.iter (fun (ic50, w) ->
      printf "%.9f %f\n" ic50 w
    ) pairs

let () = main ()
