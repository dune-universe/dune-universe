(* use jklustor in an efficient way to get the BM framework in smiles
   format for each molecule in the input.smi file, even if the input file
   contains >100k molecules *)

open Printf
open Lbvs_consent

type molecule = string
type framework = string
type filename = string

type tested = Failed
            | Ok of (molecule * framework) list

let to_do = ref 0
let already_done = ref 0

(* do we have as many frameworks as molecules *)
let test_molecules (molecules: molecule list): tested =
  let m = List.length molecules in
  (* dump molecules to a temp file *)
  let tmp_fn = MyUtils.create_tmp_filename () in
  MyUtils.write_lines molecules tmp_fn;
  (* run jklustor on them *)
  let cmd =
    sprintf "cat %s | jklustor - -v 2>&1 | grep ID | awk '{print $1}'"
      tmp_fn in
  let _exit_code, res = BatUnix.run_and_read cmd in
  let _exit_code = Sys.command (sprintf "rm -f %s" tmp_fn) in
  let frameworks = BatString.nsplit ~by:"\n" res in
  let n' = List.length frameworks in
  let frameworks, to_toss = BatList.split_at (n' - 1) frameworks in
  assert(to_toss = [""]);
  let n = List.length frameworks in
  if m <> n then
    (Log.debug "%d <> %d" m n; Failed)
  else
    (already_done := !already_done + m;
     Log.info "done: %09d / %09d" !already_done !to_do;
     Ok (BatList.combine molecules frameworks))

let rec get_frameworks (molecules: molecule list)
  : (molecule * framework) list =
  match test_molecules molecules with
  | Ok pairs -> pairs
  | Failed ->
    (* split them in two groups and start again *)
    let n = List.length molecules in
    let left, right = BatList.split_at (n / 2) molecules in
    BatList.append (get_frameworks left) (get_frameworks right)

let main () =
  Log.set_output stderr;
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let input_fn = Sys.argv.(1) in
  if not (BatString.ends_with input_fn ".smi") then
    (Log.fatal "not a .smi file: %s" input_fn;
     exit 1);
  let all_lines = MyUtils.read_lines input_fn in
  to_do := List.length all_lines;
  let frameworks = get_frameworks all_lines in
  List.iter (fun (mol, fwk) ->
      fprintf stdout "%s,%s\n" mol fwk
    ) frameworks

let () = main ()
