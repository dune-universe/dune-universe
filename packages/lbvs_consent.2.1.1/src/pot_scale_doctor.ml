(* inspect difference in ranking actives when using knowledgeable versus realist cons. policy
   FBR: versus all other policies is also interesting *)

open Printf
open Lbvs_consent

module Cons = Consent
module L = MyList
module Mol = Molecule
module Ht = Hashtbl
module StringSet = BatSet.String
module Utls = MyUtils

let memoize_actives_ranks ht score_labels =
  let rank_mols = Cons.rank_active_molecules score_labels in
  L.iter (fun (rank, (name, _score, _index, label)) ->
      assert(label);
      try
        let ranks = Ht.find ht name in
        Ht.replace ht name (rank :: ranks)
      with Not_found ->
        Ht.add ht name [rank]
    ) rank_mols

let main () =
  let query_fn = ref "" in
  let database_fn = ref "" in
  let cons_size = ref Cons.use_all_actives in
  let nb_repeats = ref 50 in
  let usage_message =
    sprintf "usage:\n%s -q queries.{csv|ecfp4|mop2d} -db database.{csv|ecfp4|mop2d} -n cons_size\n"
      Sys.argv.(0) in
  Arg.parse
    ["-q", Arg.Set_string query_fn, "<filename> queries";
     "-db", Arg.Set_string database_fn, "<filename> candidates";
     "-linpot", Arg.Unit (fun () -> Flags.potency_scaling := Flags.Linear),
     "use linear potency scaling (optional)";
     "-n", Arg.Set_int cons_size, "<int> consensus size (optional;default=all queries)";
     "-r", Arg.Set_int nb_repeats, "<int> how many times to repeat the experiment"]
    (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
    usage_message;
  let queries, candidates = Pair.map Mol.from_file (!query_fn, !database_fn) in
  assert(L.length queries >= !cons_size);
  let ps_ranks, v_ranks = Pair.map Hashtbl.create (11, 11) in
  for i = 1 to !nb_repeats do
    let ps_query =
      Cons.single_cons_query false !cons_size Policy.Knowledgeable queries
    in
    let v_query =
      let same_mols = Consensus.get_queries ps_query in
      Cons.single_cons_query false Cons.use_all_actives Policy.Realist same_mols
      (* Cons.single_cons_query false Cons.use_all_actives Policy.Opportunist same_mols *)
    in
    let ps_curves, v_curves =
      let no_BM_clusters = Ht.create 0 in
      Pair.map
        (Cons.compute_score_labels candidates queries no_BM_clusters)
        (ps_query, v_query) in
    let ps_scores, v_scores = Pair.map ROC.rank_order_by_score (ps_curves, v_curves) in
    memoize_actives_ranks ps_ranks ps_scores;
    memoize_actives_ranks v_ranks  v_scores
  done;
  (* sort actives by IC50 *)
  let actives = Mol.most_potent_first queries in
  L.iteri (fun i mol ->
      let name = Mol.get_name mol in
      try
        let ps_ranks = Ht.find ps_ranks name in
        let ps_rank = L.median ps_ranks in
        let v_ranks = Ht.find v_ranks name in
        let v_rank = L.median v_ranks in
        assert(L.length ps_ranks = L.length v_ranks);
        printf "%d %f\n" i (ps_rank -. v_rank)
      with Not_found ->
        (assert(not (Ht.mem ps_ranks name) && not (Ht.mem v_ranks name));
         eprintf "%s was used in all queries ?!\n" name)
    ) actives

let () = main ()
