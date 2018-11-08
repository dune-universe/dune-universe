
(* output distance matrix between molecules in matrix2png-compatible format *)

open Printf
open Lbvs_consent

module L = MyList

let main () =
  let input_fn = ref "" in
  let usage_message =
    sprintf "usage:\n%s -i molecules.{csv|ecfp4|mop2d}\n"
      Sys.argv.(0) in
  Arg.parse
    ["-i", Arg.Set_string input_fn,
     "<filename> where to read molecules from"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let molecules = Molecule.from_fp_file !input_fn in
  (* header line *)
  printf "#";
  L.iteri (fun i mol ->
      if i > 0 then
        printf "\t%s" (Molecule.get_name mol)
      else
        printf "%s" (Molecule.get_name mol)
    ) molecules;
  printf "\n";
  (* distances *)
  let a = Array.of_list molecules in
  let n = Array.length a in
  for i = 0 to n - 1 do
    let mi = Molecule.get_fp a.(i) in
    for j = 0 to n - 1 do
      let mj = Molecule.get_fp a.(j) in
      let d = Score.fp_tanimoto_dist mi mj in
      if j <> 0 then
        printf "\t%f" d
      else
        printf "%f" d
    done;
    printf "\n"
  done

let () = main ()
