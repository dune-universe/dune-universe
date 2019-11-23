open Printf

module Logger = Dolog.Log
module Log = Dolog.Log.Make(struct let section = "Main" end)

module Cons = Consensus
module Fp = Fingerprint
module L = MyList
module Mol = Molecule
module Pol = Policy
module StringSet = BatSet.Make(BatString)
module TopN = Top_keeper
module Utls = MyUtils

module SL = struct
  type t = string * float * int * bool
  let get_score (_, s, _, _) = s
  let get_label (_, _, _, l) = l
end

module BEDROC = Cpm.MakeROC.Make (SL)

module Vpt_point =
struct
  type t = Fp.t
  let dist = Score.fp_tanimoto_dist
end

module Vpt = Vp_tree.Make(Vpt_point)

(* useful names to constants *)
let output_all_scores = -1
let use_all_actives = -1
let no_index = -1 (* we don't care or don't have it *)
let no_label = false
let no_tversky = -1.0

type filename = string

(* this output function is for final/real users of the tool *)
let output_scores
    (out_fn: filename)
    (score_labels: Score_label.t list)
    (top_N: int): unit =
  if out_fn = "" then
    Log.warn "output_scores: OFF"
  else
    (Log.info "writing scores in %s ..." out_fn;
     Utls.with_out_file out_fn (fun out ->
         (* we output them rank-ordered by score (i.e. decreasing score) *)
         if top_N = output_all_scores then (* all of them *)
           let high_scores_first = ROC.rank_order_by_score score_labels in
           L.iter (fun (mol_name, score, _index, _label) ->
               fprintf out "%s %.4f\n" mol_name score
             ) high_scores_first
         else (* only top scoring ones *)
           let res =
             L.fold_left (fun acc (mol_name, score, _index, _label) ->
                 TopN.add mol_name score acc
               ) (TopN.create top_N) score_labels in
           let high_scores_first = TopN.high_scores_first res in
           L.iter (fun (score, name) ->
               fprintf out "%s %.4f\n" name score
             ) high_scores_first
       ))

(* the remaining output functions are for developpers/researchers and
   are useful to benchmark the performance of each consensus policy *)

let compute_cumulated_activity
    (score_labels: Score_label.t list)
    (all_actives: Mol.t list): float list =
  let _, name_to_activity = Mol.potency_scale all_actives in
  let high_scores_first = ROC.rank_order_by_score score_labels in
  let acc = ref 0.0 in
  L.map (fun (name, _score, _index, _label) ->
      let activity =
        try Hashtbl.find name_to_activity name
        with Not_found -> 0.0 in
      acc := !acc +. activity;
      !acc
    ) high_scores_first

(* output cumulated chemical diversity in terms of Bemis-Murcko clusters
   discovered among actives while we go down the rank-ordered
   list of compounds *)
let compute_chemical_diversity
    (score_labels: Score_label.t list)
    (name2cluster: (string, int) Hashtbl.t): float list =
  if Hashtbl.length name2cluster = 0 then []
  else
    let high_scores_first = ROC.rank_order_by_score score_labels in
    let clusters_seen = Hashtbl.create 11 in
    L.map (fun (name, _score, _index, is_active) ->
        if is_active then
          begin
            let m_name = Utls.remove_string_prefix "active" name in
            try
              let cluster = Hashtbl.find name2cluster m_name in
              Hashtbl.replace clusters_seen cluster ()
            with Not_found -> ()
              (* Log.warn "no cluster found for %s" m_name *)
          end;
        float (Hashtbl.length clusters_seen)
      ) high_scores_first

(* dirty way to promote scaffolds diversity:
   we keep only the highest scoring molecule for any given scaffold/BM-cluster
   FBR: keep_count should be a parameter of -fdiv *)
let force_scaffolds_diversity
    (name2cluster: (string, int) Hashtbl.t)
    (score_labels: Score_label.t list): Score_label.t list =
  let keep_count = 1 in
  if Hashtbl.length name2cluster = 0 then
    failwith "force_scaffolds_diversity: name2cluster is empty";
  let sorted = ROC.rank_order_by_score score_labels in
  let clusters_seen = Hashtbl.create 11 in
  List.filter (fun (name', _score, _index, _label) ->
      let name = Utls.remove_string_prefix "active" name' in
      match BatHashtbl.Exceptionless.find name2cluster name with
      | None -> (Log.warn "unknown cluster for %s" name'; false)
      | Some c ->
        try
          let count = Hashtbl.find clusters_seen c in
          if count >= keep_count then false
          else (Hashtbl.replace clusters_seen c (count + 1); true)
        with Not_found ->
          (Hashtbl.add clusters_seen c 1; true)
    ) sorted

let output_float_curve (verbose: bool) (out_fn: filename) (curve: float list)
  : unit =
  if not verbose || out_fn = "" then
    Log.warn "output_float_curve: OFF"
  else
    Utls.with_out_file out_fn (fun out ->
        L.iteri (fun i x ->
            fprintf out "%d %.3f\n" i x
          ) curve
      )

let output_int_curve (verbose: bool) (out_fn: filename) (curve: int list)
  : unit =
  if not verbose || out_fn = "" then
    Log.warn "output_int_curve: OFF"
  else
    Utls.with_out_file out_fn (fun out ->
        L.iteri (fun i x ->
            fprintf out "%d %d\n" i x
          ) curve
      )

type cons_id = string
type name2cluster = (string, int) Hashtbl.t
type curve =
  | AUC of cons_id * float
  | PM of cons_id * float
  | BEDROC of cons_id * float
  | Accum_actives of cons_id * float list
  | Accum_activity of cons_id * Score_label.t list * Mol.t list
  | Chemical_diversity of cons_id * Score_label.t list * name2cluster

type merged_curve =
  | AUC of cons_id * float list
  | PM of cons_id * float list
  | BEDROC of cons_id * float list
  | Accum_actives of cons_id * float list
  | Accum_activity of cons_id * float list
  | Chemical_diversity of cons_id * float list

let get_cid: curve -> cons_id = function
  | AUC (cid, _)
  | PM (cid, _)
  | BEDROC (cid, _)
  | Accum_actives (cid, _)
  | Accum_activity (cid, _, _)
  | Chemical_diversity (cid, _, _) -> cid

let get_floats: curve -> float list = function
  | AUC (_, auc) -> [auc]
  | PM (_, pm) -> [pm]
  | BEDROC (_, br) -> [br]
  | Accum_actives (_, floats) -> floats
  | Accum_activity (_, score_labels, mols) ->
    compute_cumulated_activity score_labels mols
  | Chemical_diversity (_, score_labels, map) ->
    compute_chemical_diversity score_labels map

let is_accum_activity_curve: curve -> bool = function
  | Accum_activity _ -> true
  | _ -> false

(* if the consent exe is used in production; we need to be able
   to retrieve the top scoring molecules *)
let get_score_labels (curves: curve list) =
  match List.filter is_accum_activity_curve curves with
  | [Accum_activity (_cid, score_labels, _molecules)] -> Some score_labels
  | _ -> None

(* test if two curves are similar <=> same type and consensus id *)
let similar_curves (lhs: curve) (rhs: curve): bool =
  match lhs, rhs with
  | AUC _, AUC _
  | PM _, PM _
  | BEDROC _, BEDROC _
  | Accum_actives _, Accum_actives _
  | Accum_activity _, Accum_activity _
  | Chemical_diversity _, Chemical_diversity _ -> (get_cid lhs) = (get_cid rhs)
  | AUC _, _
  | PM _, _
  | BEDROC _, _
  | Accum_actives _, _
  | Accum_activity _, _
  | Chemical_diversity _, _ -> false

let group_curves (curves: curve list): curve list list =
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
      let ok, rest = List.partition (similar_curves x) xs in
      loop ((x :: ok) :: acc) rest
  in
  loop [] curves

let merge_curves (grouped_curves: curve list): merged_curve =
  let float_lists = L.map get_floats grouped_curves in
  match grouped_curves with
  | [] -> assert(false)
  | AUC (cid, _) :: _ ->
    AUC (cid, L.concat float_lists)
  | PM (cid, _) :: _ ->
    PM (cid, L.concat float_lists)
  | BEDROC (cid, _) :: _ ->
    BEDROC (cid, L.concat float_lists)
  | Accum_actives (cid, _) :: _ ->
    Accum_actives (cid, L.nfmedian float_lists)
  | Accum_activity (cid, _, _) :: _ ->
    Accum_activity (cid, L.nfmedian float_lists)
  | Chemical_diversity (cid, _, _) :: _ ->
    Chemical_diversity (cid, L.nfmedian float_lists)

let output_curves
    (dst_dir: string) (verbose: bool) (l: merged_curve list): unit =
  L.iter (fun curve ->
      let prfx, cid, floats = match curve with
        | AUC (cid, auc) ->
          let () = match auc with
            | [x] -> Log.info "AUC: %.3f" x
            | _ -> ()
          in
          ("auc", cid, auc)
        | PM (cid, pm) ->
          let () = match pm with
            | [x] -> Log.info "PM: %.3f" x
            | _ -> ()
          in
          ("pm", cid, pm)
        | BEDROC (cid, br) ->
          let () = match br with
            | [x] -> Log.info "BEDROC: %.3f" x
            | _ -> ()
          in
          ("bedroc", cid, br)
        | Accum_actives (cid, floats) -> ("cum", cid, floats)
        | Accum_activity (cid, floats) -> ("cum_act", cid, floats)
        | Chemical_diversity (cid, floats) -> ("chem_div", cid, floats)
      in
      let out_fn = sprintf "%s/%s.%s.txt" dst_dir prfx cid in
      Log.debug "creating: %s" out_fn;
      output_float_curve verbose out_fn floats
    ) l

(* remove molecules used in the cons. query from the DB we screen
   (makes dataset harder) *)
let remove_queries_from_database cons_q candidates' =
  let query_names = Cons.get_query_names cons_q in
  L.filter (fun mol ->
      let name = Mol.get_name mol in
      if StringSet.mem name query_names then
        (Log.warn "rm %s from DB" name; false)
      else
        true
    ) candidates'

(* this one is not a consensus and hence requires special care *)
let single_query
    (candidates': Mol.t list)
    (all_actives: Mol.t list)
    (name2cluster: (string, int) Hashtbl.t)
    (cons_q: Cons.t): curve list =
  let candidates = remove_queries_from_database cons_q candidates' in
  match cons_q with
  | Cons.Single _ ->
    let cstr = Cons.to_string cons_q in
    let csize = Cons.size cons_q in
    let cons_id = sprintf "%s.%03d" cstr csize in
    let tani_or_tver = Score.get_fp_score !Flags.curr_score in
    let score_fun =
      (fun (q: Fp.t) (cand: Mol.t) ->
         let cand_fp = Mol.get_fp cand in
         let score = tani_or_tver q cand_fp in
         let label = Mol.is_active cand in
         Mol.(cand.name, score, no_index, label))
    in
    let queries = Cons.get_fprints cons_q in
    let curves: curve list list =
      List.map (fun q ->
          let score_labels = List.map (score_fun q) candidates in
          let auc, cum_curve = ROC.auc score_labels in
          let bedroc = BEDROC.bedroc_auc score_labels in
          let pm = ROC.power_metric !Flags.pm_percent score_labels in
          let avg_cum_curve = L.map float cum_curve in
          (AUC (cons_id, auc): curve) ::
          PM (cons_id, pm) ::
          BEDROC (cons_id, bedroc) ::
          Accum_actives (cons_id, avg_cum_curve) ::
          Accum_activity (cons_id, score_labels, all_actives) ::
          [Chemical_diversity (cons_id, score_labels, name2cluster)]
        ) queries in
    L.concat curves
  | _ -> assert(false)

let oppo_scorer
    (tani_or_tver: Fp.t -> Fp.t -> float)
    (tap_fun: Mol.t -> unit)
    (fprints: Fp.t list)
    (cand: Mol.t): Score_label.t =
  (* min rank/max score: each candidate molecule is assigned
     the maximum score it gets over all queries *)
  tap_fun cand;
  let cand_name = Mol.get_name cand in
  let cand_fp = Mol.get_fp cand in
  let scores =
    L.map (fun q ->
        tani_or_tver q cand_fp
      ) fprints
  in
  let score = L.max scores in
  let label = Mol.is_active cand in
  (cand_name, score, no_index, label)

let index_queries (fprints: Fp.t list): Vpt.t =
  assert(!Flags.curr_score = Flags.Tanimoto);
  Vpt.create fprints

let fast_oppo_scorer
    (tap_fun: Mol.t -> unit)
    (vpt: Vpt.t)
    (cand: Mol.t): Score_label.t =
  tap_fun cand;
  let cand_name = Mol.get_name cand in
  let cand_fp = Mol.get_fp cand in
  let nearest_tdist, _nearest_fp =
    Vpt.nearest_neighbor cand_fp vpt in
  let score = 1.0 -. nearest_tdist in
  let label = Mol.is_active cand in
  (cand_name, score, no_index, label)

let cons_scorer (score_fun: Fp.t -> float) (tap_fun: Mol.t -> unit) (cand: Mol.t)
  : Score_label.t =
  tap_fun cand;
  let cand_name = Mol.get_name cand in
  let cand_fp = Mol.get_fp cand in
  let score = score_fun cand_fp in
  let label = Mol.is_active cand in
  (cand_name, score, no_index, label)

(* give a score to each candidate molecule given a consensus query *)
let compute_score_labels
    (candidates': Mol.t list)
    (name2cluster: (string, int) Hashtbl.t)
    (cons_q: Cons.t): Score_label.t list =
  let candidates = remove_queries_from_database cons_q candidates' in
  let score_fun: (Mol.t -> Score_label.t) =
    match cons_q with
    | Cons.Single _ -> assert(false)
    | Cons.Opportunist (_queries, fprints) ->
      if !Flags.fast_oppo_score then
        (Log.info "fast oppo score";
         fast_oppo_scorer ignore (index_queries fprints))
      else
        let tani_or_tver = Score.get_fp_score !Flags.curr_score in
        oppo_scorer tani_or_tver ignore fprints
    | Cons.Knowledgeable_ecfp4 _
    | Cons.Knowledgeable_maccs _
    | Cons.Knowledgeable_mop2d _
    | Cons.Optimist_ecfp4 _
    | Cons.Optimist_maccs _
    | Cons.Optimist_mop2d _
    | Cons.Realist_ecfp4 _
    | Cons.Realist_maccs _
    | Cons.Realist_mop2d _ -> cons_scorer (Cons.cons_score cons_q) ignore
  in
  let score_labels' = List.map score_fun candidates in
  if !Flags.force_scaffolds_diversity then
    force_scaffolds_diversity name2cluster score_labels'
  else
    score_labels'

(* assign its rank to each active molecule *)
let rank_active_molecules score_labels =
  L.fold_lefti (fun acc i sl ->
      if Score_label.is_active sl then (i, sl) :: acc
      else acc
    ) [] (ROC.rank_order_by_score score_labels)

let query
    (candidates': Mol.t list)
    (all_actives: Mol.t list)
    (name2cluster: (string, int) Hashtbl.t)
    (cons_q: Cons.t): curve list =
  if Cons.is_single cons_q then
    single_query candidates' all_actives name2cluster cons_q
  else
    let cstr = Cons.to_string cons_q in
    let csize = Cons.size cons_q in
    let cons_id = sprintf "%s.%03d" cstr csize in
    let score_labels = compute_score_labels candidates' name2cluster cons_q in
    let auc, cum_curve = ROC.auc score_labels in
    (* 10% is for NRLIST, for VUQSAR we need much less *)
    let pm = ROC.power_metric 0.1 score_labels in
    let avg_cum_curve = L.map float cum_curve in
    AUC (cons_id, auc) ::
    PM (cons_id, pm) ::
    Accum_actives (cons_id, avg_cum_curve) ::
    Accum_activity (cons_id, score_labels, all_actives) ::
    [Chemical_diversity (cons_id, score_labels, name2cluster)]

(* give a score to each candidate molecule given a consensus query;
   uses less memory than the above query function and scales better with
   nprocs *)
let pcons_query
    (cons_q: Cons.t)
    (database_fn: filename)
    (scores_out_fn: filename)
    (nprocs: int)
    (top_N: int): unit =
  Log.info "reading candidates from %s ..." database_fn;
  let output, tap_fun =
    if BatString.ends_with database_fn ".bin" || nprocs > 1 then
      (* don't overwrite it and don't attempt to write in parallel
           in the same file ... *)
      (None, ignore)
    else
      let bin_out_fn = database_fn ^ ".bin" in
      (* gzip -1 _does_ better than lz4 on ocaml Marshal output
         in my tests. It compresses faster and better! *)
      let output = Gzip.open_out ~level:1 bin_out_fn in
      (Some output, Mol.to_chan output) in
  let score_molecule =
    match cons_q with
    | Cons.Single _ -> failwith "pcons_query: single strat. not supported"
    | Cons.Knowledgeable_ecfp4 _
    | Cons.Knowledgeable_maccs _
    | Cons.Knowledgeable_mop2d _
    | Cons.Optimist_ecfp4 _
    | Cons.Optimist_maccs _
    | Cons.Optimist_mop2d _
    | Cons.Realist_ecfp4 _
    | Cons.Realist_maccs _
    | Cons.Realist_mop2d _ -> cons_scorer (Cons.cons_score cons_q) tap_fun
    | Cons.Opportunist (_queries, fprints) ->
      if !Flags.fast_oppo_score then
        (Log.info "fast oppo score";
         fast_oppo_scorer tap_fun (index_queries fprints))
      else
        let tani_or_tver = Score.get_fp_score !Flags.curr_score in
        oppo_scorer tani_or_tver tap_fun fprints
  in
  begin
    if nprocs > 1 || not (BatString.ends_with database_fn ".bin") then
      let mol_reader =
        Mol.mol_reader_of_filename_fp !Flags.curr_fingerprint in
      Log.warn "sorted scores (will not run in constant memory)";
      let score_labels =
        Utls.parmap_on_file nprocs database_fn score_molecule mol_reader in
      output_scores scores_out_fn score_labels top_N
    else
      (* sequentially process molecules from a compressed binary file *)
      let input = Gzip.open_in database_fn in
      Utls.with_out_file scores_out_fn (fun out ->
          let top_scores = ref (TopN.create top_N) in
          let count = ref 0 in
          let total = ref 0 in
          if top_N = output_all_scores then
            Log.warn "scores are unsorted (to run in constant memory)";
          try
            while true do
              let mol = Mol.from_chan input in
              incr count;
              if !count = 50_000 then
                (total := !total + !count;
                 count := 0;
                 eprintf "scored %d molecules\r%!" !total; (* user feedback *)
                );
              let mol_name, score, _index, _label = score_molecule mol in
              if top_N = output_all_scores then
                fprintf out "%s %.4f\n" mol_name score
              else
                top_scores := TopN.add mol_name score !top_scores
            done;
          with End_of_file ->
            begin
              eprintf "\n"; (* finish feedback *)
              let high_scores_first = TopN.high_scores_first !top_scores in
              Gzip.close_in input;
              L.iter (fun (score, name) ->
                  fprintf out "%s %.4f\n" name score
                ) high_scores_first
            end
        )
  end;
  Utls.may_apply Gzip.close_out output

let shuffle_then_cut size mols =
  let shuffled = L.real_shuffle mols in
  fst (L.split_at size shuffled)

(* sort by IC50, then cut in slices, then shuffle and take one from each slice *)
let split_then_shuffle size mols' =
  let mols = Mol.most_potent_first mols' in
  let n = List.length mols in
  let slice_size = int_of_float (floor (float n /. float size)) in
  let all_slices = L.ntake slice_size mols in
  let enough_slices = fst (L.split_at size all_slices) in
  let res = L.map (shuffle_then_cut 1) enough_slices in
  L.flatten res

(* randomly chooses 'size' molecules from 'mols' until we have
   reached the 'max' number of random choices;
   in case potency scaling is used, we need to sample molecules
   in a smart way to ensure the whole potency scale is covered *)
let random_choices smart_IC50_sampling size max mols =
  if size = use_all_actives then [mols]
  else
    let sampler =
      if smart_IC50_sampling then
        split_then_shuffle
      else
        shuffle_then_cut
    in
    MyUtils.n_times max (fun () ->
        let res = sampler size mols in
        assert(L.length res = size);
        res)

(* create 'max' cons queries of 'size' using all possible strategies *)
let scan_policies size max mols =
  (* all consensus policies must be run with the same set of actives *)
  let chosen = random_choices false size max mols in
  let n = L.length chosen in
  if n < max then
    Log.warn "scan_policies: not enough queries: %d < %d" n max;
  List.flatten (L.map (Cons.create Pol.Single) chosen ::
                L.map (Cons.create Pol.Opportunist) chosen ::
                L.map (Cons.create Pol.Optimist) chosen ::
                L.map (Cons.create Pol.Realist) chosen :: [])

(* create 'max' cons queries of size 'size' each, using the given 'strategy' *)
let create_cons_queries verbose size (strat: Pol.t) max mols =
  let chosen = random_choices (strat = Pol.Knowledgeable) size max mols in
  let res = L.map (Cons.create ~verbose:verbose strat) chosen in
  let n = L.length res in
  if n < max then
    Log.warn "create_cons_queries: not enough queries: %d < %d" n max
  ;
  res

let single_cons_query verbose size strat mols =
  match create_cons_queries verbose size strat 1 mols with
  | [one] -> one
  | _ -> assert(false)

let main () =
  let start = Unix.gettimeofday () in
  Logger.set_log_level Logger.INFO;
  Logger.set_output stderr;
  Logger.color_on ();
  (* mandatory options *)
  let strat = ref "" in
  let query_fn = ref "" in
  let database_fn = ref "" in
  let clusters_fn = ref "" in
  let scores_out_fn = ref "/dev/stdout" in
  let output_top_N = ref output_all_scores in
  let cons_size = ref use_all_actives in
  let downsize = ref 1.0 in
  (* optional ones *)
  let nb_queries = ref 1 in
  let tversky_alpha = ref no_tversky in
  let verbose = ref false in
  let scan_all_policies = ref false in
  let randomize_DB = ref false in
  let nprocs = ref 1 in
  let strategies_str = "{sing|oppo|opti|real|know}" in
  let usage_message =
    sprintf
      "usage:\n%s -s %s \
       -q queries.{sdf|mol2|csv|ecfp4|maccs|pubc} \
       -db candidates.{sdf|mol2|csv|ecfp4|maccs|pubc}\n"
      Sys.argv.(0) strategies_str in
  let argc = Array.length Sys.argv in
  if argc = 1 then
    let () = eprintf "%s" usage_message in
    let _ = exit 1 in
    () (* for typing *)
  else
    Arg.parse
      ["-s", Arg.Set_string strat,
       sprintf "<strat> consensus strategy %s (mandatory)" strategies_str;
       "-tani", Arg.Unit (fun () -> Flags.curr_score := Flags.Tanimoto),
       "use Tanimoto score (default)";
       "-tver", Arg.Set_float tversky_alpha,
       "<float> use Tversky score with given alpha weight instead \
        of Tanimoto (optional)";
       "-pm", Arg.Set_float Flags.pm_percent,
       sprintf "<float> Power Metric threshold (optional; default=%.2f)"
         !Flags.pm_percent;
       "-q", Arg.Set_string query_fn,
       "<filename> queries file (known actives; mandatory)";
       "-c", Arg.Set_string clusters_fn,
       "<filename> mapping of molecule name to Bemis-Murcko framework and \
        corresponding cluster (optional) to create chemical diversity curves";
       "-dsize", Arg.Set_float downsize,
       "<float> downsize dataset by factor (x in ]0.0;1.0]; optional)";
       "-fdiv", Arg.Set Flags.force_scaffolds_diversity,
       "force scaffolds diversity (experimental, optional)";
       "-db", Arg.Set_string database_fn,
       "<filename> database to rank order (mandatory)";
       "-linpot", Arg.Unit (fun () -> Flags.potency_scaling := Flags.Linear),
       "use linear potency scaling (optional; experimental)";
       "-o", Arg.Set_string scores_out_fn,
       "<filename> where to write scores (can be combined with -top)";
       "-n", Arg.Set_int cons_size,
       "<int> consensus size; #known actives used to create query (optional; \
        default=all molecules in query file)";
       "-top", Arg.Set_int output_top_N,
       "<int> how many top scoring molecules to write out \
        (optional; default=all; must be combined with -o)";
       "-np", Arg.Set_int nprocs,
       "<int> number of cores to use (default=1; optional)";
       "-max", Arg.Set_int nb_queries,
       "<int> number of distinct queries to run for statistics \
        (default=1; optional; developer)";
       "-randDB", Arg.Set randomize_DB,
       "randomize DB prior to screening (optional)";
       "-scan", Arg.Set scan_all_policies,
       "scan all policies (optional; developer)";
       "-fast", Arg.Set Flags.fast_oppo_score,
       "index queries before oppo strat tanimoto scoring (optional)";
       "-v", Arg.Set verbose, "verbose/debug mode (optional)"]
      (fun arg -> raise (Arg.Bad ("Bad argument: " ^ arg)))
      usage_message;
  if !verbose then Logger.set_log_level Logger.DEBUG;
  if !tversky_alpha <> no_tversky then
    Flags.curr_score := Flags.Tversky !tversky_alpha;
  if !Flags.force_scaffolds_diversity && !clusters_fn = "" then
    failwith "-fdiv requires -c";
  let name2cluster =
    if !clusters_fn <> "" then Mol.load_clusters !clusters_fn
    else Hashtbl.create 0 in
  let query_mols, candidate_mols =
    if !downsize = 1.0 then
      Pair.map Mol.from_file (!query_fn, !database_fn)
    else
      Mol.from_downsized_file !downsize !database_fn in
  let candidate_mols =
    (if !randomize_DB then L.real_shuffle else Utls.id) candidate_mols in
  if L.length query_mols < !cons_size then
    Log.warn "not enough molecules in %s" !query_fn
  ;
  let nb_molecules = List.length candidate_mols in
  let cons_queries =
    if !scan_all_policies then
      (* try all policies *)
      scan_policies !cons_size !nb_queries query_mols
    else
      (* use a single policy *)
      let strategy = Pol.of_string !strat in
      create_cons_queries !verbose !cons_size strategy !nb_queries query_mols
  in
  let nb_screens = L.sum (L.map Cons.length cons_queries) in
  Log.info "nb_screens: %d" nb_screens;
  let curves' =
    Utls.list_parmap !nprocs
      (query candidate_mols query_mols name2cluster) cons_queries
  in
  let curves = L.concat curves' in
  (* write scores out (will only work if single query) *)
  Utls.may_apply
    (fun scores -> output_scores !scores_out_fn scores !output_top_N)
    (get_score_labels curves);
  (* group curves *)
  let grouped_curves = group_curves curves in
  (* merge those that need *)
  let merged_curves = L.map merge_curves grouped_curves in
  (* write them out *)
  let dst_dir = Filename.dirname !database_fn in
  output_curves dst_dir !verbose merged_curves;
  let stop = Unix.gettimeofday () in
  let delta_t = stop -. start in
  Log.info "processed %.3f molecule/s" ((float nb_molecules) /. delta_t)
