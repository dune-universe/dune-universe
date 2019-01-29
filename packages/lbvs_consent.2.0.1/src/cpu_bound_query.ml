(* compare performance of opti cons. policy Vs oppo cons. policy
   in a time-limited experiment; opti with 10 actives is two times faster
   than oppo on binary files; hence we will allow opti to screen the whole DB
   but oppo will only be allowed to screen a random half split of the DB *)

open Printf
open Lbvs_consent

module Cons = Consent
module L = MyList
module Mol = Molecule
module Ht = Hashtbl
module Utls = MyUtils

(* simulates the fact that the oppo strat can hinder the exploration of the chemical
   space (if scoring is the limiting factor) compared to a faster cons. policy *)
let shuffle_then_truncate l speedup =
  let shuffled = L.real_shuffle l in
  let n = float (L.length l) in
  let m = int_of_float (Utls.round (n /. speedup)) in
  L.take m shuffled

let main () =
  Log.set_log_level Log.INFO;
  Log.set_output stderr;
  Log.color_on ();
  let query_fn = ref "" in
  let database_fn = ref "" in
  let cons_size = ref 10 in
  let nb_repeats = ref 50 in
  let no_BM_clusters = Ht.create 0 in
  let usage_message =
    sprintf
      "usage:\n%s -q queries.{csv|ecfp4|mop2d} -db database.{csv|ecfp4|mop2d} -n cons_size -r nb_repeats\n"
      Sys.argv.(0) in
  Arg.parse
    ["-q", Arg.Set_string query_fn, "<filename> queries";
     "-db", Arg.Set_string database_fn, "<filename> candidates";
     "-n", Arg.Set_int cons_size,
     sprintf "<int> consensus size (optional;default=%d)" !cons_size;
     "-r", Arg.Set_int nb_repeats,
     sprintf "<int> how many times to repeat the experiment (optional;default=%d)" !nb_repeats]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let queries, candidates = Pair.map Mol.from_file (!query_fn, !database_fn) in
  assert(L.length queries >= !cons_size);
  let oppo_curves = ref [] in
  let opti_curves = ref [] in
  let speedup = match !cons_size with
    (* those speedups were precisely measured in benchmarks *)
    | 5 -> 1.46 (* means opti goes 1.46 times faster than oppo *)
    | 10 -> 2.05
    | 20 -> 3.19
    | n -> failwith (sprintf "doesn't know speedup for cons. size %d" n)
  in  
  for i = 1 to !nb_repeats do
    let some_candidates = shuffle_then_truncate candidates speedup in
    let oppo_query =
      Cons.single_cons_query false !cons_size Policy.Opportunist queries
    in
    let opti_query =
      let same_mols = Consensus.get_queries oppo_query in
      Cons.single_cons_query false Cons.use_all_actives Policy.Optimist same_mols
    in
    let oppo_curve = Cons.compute_score_labels some_candidates queries no_BM_clusters oppo_query in
    let opti_curve = Cons.compute_score_labels candidates      queries no_BM_clusters opti_query in
    let oppo_scores, opti_scores = Pair.map ROC.rank_order_by_score (oppo_curve, opti_curve) in
    let oppo_actives_curve, opti_actives_curve =
      Pair.map ROC.cumulated_number_actives (oppo_scores, opti_scores) in
    Utls.push oppo_actives_curve oppo_curves;
    Utls.push opti_actives_curve opti_curves;
  done;
  (* compute median curves *)
  let med_oppo_curve, med_opti_curve = Pair.map L.robust_nmedian (!oppo_curves, !opti_curves) in
  let min_len = min (L.length med_oppo_curve) (L.length med_opti_curve) in
  (* output curves *)
  printf "#oppo_cum_actives opti_cum_actives\n";
  L.iter2 (fun x y ->
      printf "%f %f\n" x y
    ) (L.take min_len med_oppo_curve) (L.take min_len med_opti_curve)

let () = main ()
