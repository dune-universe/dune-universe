(* test effect of consensus size on AUC and power metric at 1% *)

open Printf
open Lbvs_consent

module Cons = Consent
module L = MyList
module Mol = Molecule
module Ht = Hashtbl
module Utls = MyUtils

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let query_fn = ref "" in
  let database_fn = ref "" in
  let cons_size = ref (-1) in
  let nb_repeats = ref 10 in
  let no_BM_clusters = Ht.create 0 in
  let usage_message =
    sprintf
      "usage:\n%s -q queries.{csv|ecfp4|mop2d} -db database.{csv|ecfp4|mop2d} \
       -n cons_size -r nb_repeats\n"
      Sys.argv.(0) in
  Arg.parse
    ["-q", Arg.Set_string query_fn, "<filename> queries";
     "-db", Arg.Set_string database_fn, "<filename> candidates";
     "-n", Arg.Set_int cons_size, "<int> consensus size (mandatory)";
     "-r", Arg.Set_int nb_repeats,
     sprintf
       "<int> how many times to repeat the experiment (optional;default=%d)"
       !nb_repeats]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let queries, candidates = Pair.map Mol.from_file (!query_fn, !database_fn) in
  assert(L.length queries >= !cons_size);
  let aucs, pms = ref [], ref [] in
  for i = 1 to !nb_repeats do
    let candidates = L.real_shuffle candidates in
    let opti_query =
      Cons.single_cons_query false !cons_size Policy.Optimist queries
    in
    let score_labels =
      Cons.compute_score_labels candidates queries no_BM_clusters opti_query in
    let auc, _cum_curve = ROC.auc score_labels in
    let pm = ROC.power_metric 0.01 score_labels in
    Utls.push auc aucs;
    Utls.push pm pms;
  done;
  (* compute median curves *)
  let (med_auc, med_auc_mad),
      (med_pm, med_pm_mad) = Pair.map L.mad (!aucs, !pms) in
  (* output curves *)
  printf "cons_size: %d AUC: %f AUC_MAD: %f PM1%%: %f PM_MAD: %f\n"
    !cons_size med_auc med_auc_mad med_pm med_pm_mad

let () = main ()
