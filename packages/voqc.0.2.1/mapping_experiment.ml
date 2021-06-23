open Printf
open Voqc.Qasm
open Voqc.Main

(* Code for evaluating VOQC mapping & optimization

   Run with a directory as input (-d). Produces a CSV output "out.csv" with
   gate counts for different configurations.
*)

let optimize c = optimize_ibm (optimize_nam c)

let run_voqc d oc inf =
  let _ = printf "Processing %s%!\n" inf in
  let (c, n) = read_qasm (Filename.concat d inf) in
  let c = convert_to_rzq c in
  let cg = make_grid 6 6 in
  let la = trivial_layout n in
  (* map only *)
  let (c0, _) = simple_map c la cg in
  (* optimize -> map *)
  let c1 = optimize c in
  let (c1, _) = simple_map c1 la cg in
  (* map -> optimize *)
  let (c2, _) = simple_map c la cg in
  let c2 = optimize c2 in
  (* optimize -> map -> optimize *)
  let c3 = optimize_nam c in
  let (c3, _) = simple_map c3 la cg in
  let c3 = optimize c3 in
  (* write output *)
  fprintf oc "%s,%d,%d,%d,%d,%d\n" inf (total_gate_count c) (total_gate_count c0) 
    (total_gate_count c1) (total_gate_count c2) (total_gate_count c3)

(* Argument parsing *)
let d = ref ""
let usage = "usage: " ^ Sys.argv.(0) ^ " -d string"
let speclist = [
    ("-d", Arg.Set_string d, ": directory with input programs");
  ]
let () =
  Arg.parse
    speclist
    (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
    usage;
if !d = "" then printf "ERROR: Input directory (-d) required.\n" else 
let fs = Sys.readdir !d in
let oc = open_out "out.csv" in
let _ = fprintf oc "Filename,Original,Map,Optimize->Map,Map->Optimize,Optimize->Map->Optimize\n" in
let _ = Array.iter (run_voqc !d oc) fs in
close_out oc
