(* compare chemical diversity obtained when forcing diversity or not *)

open Printf
open Lbvs_consent

module Cons = Consent
module L = MyList
module Mol = Molecule
module Ht = Hashtbl
module Utls = MyUtils

(* FBR: test for oppo strat too *)

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let query_fn = ref "" in
  let clusters_fn = ref "" in
  let database_fn = ref "" in
  let cons_size = ref 10 in
  let nb_repeats = ref 50 in
  let no_BM_clusters = Ht.create 0 in
  let quiet = false in
  let usage_message =
    sprintf
      "usage:\n%s -q queries.{csv|ecfp4|mop2d} -db database.{csv|ecfp4|mop2d} -n cons_size -r nb_repeats\n"
      Sys.argv.(0) in
  Arg.parse
    ["-c", Arg.Set_string clusters_fn,
     "<filename> mapping of molecule name to Bemis-Murcko framework and corresponding cluster";
      "-q", Arg.Set_string query_fn, "<filename> queries";
     "-db", Arg.Set_string database_fn, "<filename> candidates";
     "-n", Arg.Set_int cons_size,
     sprintf "<int> consensus size (optional;default=%d)" !cons_size;
     "-r", Arg.Set_int nb_repeats,
     sprintf "<int> how many times to repeat the experiment (optional;default=%d)" !nb_repeats]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let queries, candidates = Pair.map Mol.from_file (!query_fn, !database_fn) in
  assert(L.length queries >= !cons_size);
  assert(!nb_repeats >= 1);
  let name2cluster = Mol.load_clusters !clusters_fn in
  let std_curves = ref [] in
  let fdiv_curves = ref [] in
  for i = 1 to !nb_repeats do
    let candidates = L.real_shuffle candidates in
    let query = Cons.single_cons_query quiet !cons_size Policy.Optimist queries in
    let std_curve = Cons.compute_score_labels candidates queries no_BM_clusters query in
    let fdiv_curve = Cons.force_scaffolds_diversity name2cluster std_curve in
    let std_chem_div = Cons.compute_chemical_diversity std_curve name2cluster in
    let fdiv_chem_div = Cons.compute_chemical_diversity fdiv_curve name2cluster in
    Utls.push std_chem_div std_curves;
    Utls.push fdiv_chem_div fdiv_curves;
  done;
  (* compute median curves *)
  let std_med_curve, fdiv_med_curve = Pair.map L.robust_nfmedian (!std_curves, !fdiv_curves) in
  let fdiv_max = L.max fdiv_med_curve in
  let len_diff = L.length std_med_curve - L.length fdiv_med_curve in
  let fdiv_med_curve = L.pad fdiv_med_curve len_diff fdiv_max in
  (* output curves *)
  printf "#std_chem_div fdiv_chem_div\n";
  List.iter2 (fun x y ->
      printf "%f %f\n" x y
    ) std_med_curve fdiv_med_curve

let () = main ()
