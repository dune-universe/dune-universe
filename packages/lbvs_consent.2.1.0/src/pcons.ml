open Printf
open Lbvs_consent

module Logger = Log
module Log = Log.Make(struct let section = "Main" end)

module Fp = Fingerprint
module L = MyList
module Mol = Molecule
module Pol = Policy
module Utls = MyUtils

(* constant memory and parallel version of consent *)

let main () =
  Logger.set_log_level Logger.INFO;
  Logger.set_output stderr;
  Logger.color_on ();
  (* options turned off/fixed *)
  let cons_size = Consent.use_all_actives in
  let one_query = 1 in
  (* mandatory options *)
  let tversky_alpha = ref Consent.no_tversky in
  let strat = ref "" in
  let use_maccs_fp = ref false in
  let use_ecfp4_fp = ref true in
  let query_fn = ref "" in
  let database_fn = ref "" in
  let scores_out_fn = ref "/dev/stdout" in
  let verbose = ref false in
  let nprocs = ref 1 in
  let top_N = ref Consent.output_all_scores in
  let strategies_str = "{oppo|opti|real|know}" in
  let usage_message =
    sprintf
      "usage:\n%s -s %s \
       -q queries.{sdf|mol2|csv|ecfp4} -db candidates.{sdf|mol2|csv|ecfp4} \
       -o scores.csv\n"
      Sys.argv.(0) strategies_str in
  let argc = Array.length Sys.argv in
  if argc = 1 then
    (eprintf "%s" usage_message; exit 1)
  else
    Arg.parse
      ["-s", Arg.Set_string strat,
       sprintf "<strat> consensus strategy %s (mandatory)" strategies_str;
       "-q", Arg.Set_string query_fn,
       "<filename> queries file (known actives; mandatory)";
       "-tani", Arg.Unit (fun () -> Flags.curr_score := Flags.Tanimoto),
       "use Tanimoto score (default)";
       "-tver", Arg.Set_float tversky_alpha,
       "<float> use Tversky score with given alpha weight instead \
        of Tanimoto (optional)";
       "-maccs", Arg.Set use_maccs_fp,
       "use MACCS fingerprints";
       "-ecfp4", Arg.Set use_ecfp4_fp,
       "use unfolded ECFP4 fingerprints (default)";
       "-db", Arg.Set_string database_fn,
       "<filename> database to rank order (mandatory)";
       "-o", Arg.Set_string scores_out_fn,
       "<filename> where to write scores (default=stdout)";
       "-np", Arg.Set_int nprocs,
       "<int> number of cores to use (default=1; optional)";
       "--fast", Arg.Set Flags.fast_oppo_score,
       "index queries before oppo strat tanimoto scoring \
        (optional,experimental)";
       "-top", Arg.Set_int top_N,
       "<int> how many best scoring molecules to write out (default=all)";
       "-v", Arg.Set verbose, "verbose/debug mode (optional)"]
      (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
      usage_message;
  (* setup options *)
  if !verbose then Logger.set_log_level Logger.DEBUG;
  if !use_maccs_fp then
    (Flags.curr_fingerprint := Flags.MACCS;
     use_ecfp4_fp := false);
  if !use_ecfp4_fp then
    (Flags.curr_fingerprint := Flags.ECFP4;
     use_maccs_fp := false);
  if !tversky_alpha <> Consent.no_tversky then
    Flags.curr_score := Flags.Tversky !tversky_alpha;
  (* check some options combinations *)
  assert(not (!use_maccs_fp && !use_ecfp4_fp));
  let query_mols = Mol.from_file_fp !Flags.curr_fingerprint !query_fn in
  let cons_queries =
    (* use a single policy *)
    let strategy = Pol.of_string !strat in
    Consent.create_cons_queries
      !verbose cons_size strategy one_query query_mols
  in
  match cons_queries with
  | [] -> assert(false)
  | [cons_q] -> (* this is supposed to run a single query *)
    Consent.pcons_query
      cons_q
      !database_fn
      !scores_out_fn
      !nprocs
      !top_N
  | _ -> assert(false)

let () = main ()
