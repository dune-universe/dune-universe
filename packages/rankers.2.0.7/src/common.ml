(* Copyright (C) 2019, Francois Berenger

   Yamanishi laboratory,
   Department of Bioscience and Bioinformatics,
   Faculty of Computer Science and Systems Engineering,
   Kyushu Institute of Technology,
   680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan. *)

(* functionalities common to several modules *)

open Printf

module A = MyArray
module FpMol = Molenc.FpMol
module FloatMap = BatMap.Float
module L = MyList
module Log = Dolog.Log
module Ht = BatHashtbl
module SL = Indexed_mol.SL
module Top = Cpm.TopKeeper

module ROC = Cpm.MakeROC.Make(SL)

let normalize_scores score_labels =
  let all_scores = L.map SL.get_score score_labels in
  let min_score, max_score = L.min_max all_scores in
  let delta_score = max_score -. min_score in
  let normalize s =
    (s -. min_score) /. delta_score in
  L.map (fun (n, s) -> (n, normalize s)) score_labels

let mol_of_line (i, l) =
  try FpMol.parse_one i l
  with exn ->
    let () = Log.fatal "Common.mol_of_line: invalid line %d: %s" i l in
    raise exn

let sum_contribs kernel bwidth mols_bst test_mol =
  let nearby = Bstree.neighbors test_mol bwidth mols_bst in
  L.fold_left (fun acc near ->
      let d = FpMol.dist test_mol near in
      (Kernel.eval kernel bwidth d) +. acc
    ) 0.0 nearby

let sum_contribs_single kernel bwidth mols_bst test_mol =
  let nearby_mols = Bstree.neighbors test_mol bwidth mols_bst in
  L.fold_left (fun (act_contribs, dec_contribs) near_mol ->
      let d = FpMol.dist test_mol near_mol in
      let contrib = Kernel.eval kernel bwidth d in
      if FpMol.is_active near_mol then
        (act_contribs +. contrib, dec_contribs)
      else
        (act_contribs, dec_contribs +. contrib)
    ) (0.0, 0.0) nearby_mols

(* KDE/PDE at test_mol *)
let kde kernel n_mols bwidth mols_bst test_mol =
  let sum = sum_contribs kernel bwidth mols_bst test_mol in
  sum /. (float n_mols)

let score_one_single_raw kernel n_acts n_decs bwidth mols_bst test_mol =
  let act_contribs, dec_contribs =
    sum_contribs_single kernel bwidth mols_bst test_mol in
  let act_contrib = act_contribs /. (float n_acts) in
  let dec_contrib = dec_contribs /. (float n_decs) in
  (act_contrib, dec_contrib)

(* ranking score using KDE *)
let score_one_single kernel n_acts n_decs bwidth mols_bst test_mol =
  let name = FpMol.get_name test_mol in
  let act_contrib, dec_contrib =
    score_one_single_raw kernel n_acts n_decs bwidth mols_bst test_mol in
  let score = Utls.score_mol act_contrib dec_contrib in
  SL.create name score

(* variables to monitor optimization progress *)
let nb_iter = ref 0
let curr_best = ref 0.0

let reset_iter_and_auc () =
  nb_iter := 0;
  curr_best := 0.0

let eval_solution_indexed ncores kernel indexed_mols params _gradient =
  (* we don't have a gradient => _gradient *)
  let bwidth = params.(0) in
  let score_labels =
    Parany.Parmap.parmap ncores (
      Indexed_mol.score kernel bwidth
    ) indexed_mols in
  let perf_metric =
    (match !Flags.optim_target with
     | ROC_AUC -> ROC.auc
     | PR_AUC -> ROC.pr_auc
    ) score_labels in
  curr_best := max !curr_best perf_metric; (* best AUC seen up to now *)
  if !Flags.verbose || !nb_iter mod 10 = 0 then
    Log.info "%02d %f (%.3f)=%f"
      !nb_iter !curr_best bwidth perf_metric
  ;
  incr nb_iter;
  perf_metric

let eval_solution_indexed_brute kernel indexed_mols bwidth =
  let score_labels = L.map (Indexed_mol.score kernel bwidth) indexed_mols in
  (match !Flags.optim_target with
   | ROC_AUC -> ROC.auc
   | PR_AUC -> ROC.pr_auc) score_labels

let optimize_global_bandwidth max_evals ncores kernel indexed_mols =
  reset_iter_and_auc ();
  Nlopt.(
    (* local optimizer that will be passed to the global one *)
    let local = create sbplx 1 in (* local, gradient-free *)
    set_max_objective local
      (eval_solution_indexed ncores kernel indexed_mols);
    (* I don't set parameter bounds on the local optimizer, I guess
     * the global optimizer handles this *)
    (* hard/stupid stop conditions *)
    set_stopval local 1.0; (* max AUC *)
    (* smart stop conditions *)
    set_ftol_abs local 0.0001;
    (* I also do not provide an initial guess for the local optimizer *)
    (* global optimizer that will use the local one *)
    let global = create auglag 1 in (* global *)
    set_local_optimizer global local;
    set_max_objective global
      (eval_solution_indexed ncores kernel indexed_mols);
    (* bandwidth is in [0..1] *)
    set_lower_bounds global [| 0.01 |];
    set_upper_bounds global [| 1.0 |];
    (* hard/stupid stop conditions *)
    set_stopval global 1.0; (* max AUC *)
    set_maxeval global max_evals; (* max number of AUC calls *)
    let initial_guess = [| 0.5 |] in
    let stop_cond, params, val_auc = optimize global initial_guess in
    Log.info "optimize_global_bandwidth: %s" (string_of_result stop_cond);
    let kb = params.(0) in
    (kb, val_auc)
  )

let index_molecules ncores training_set validation_set =
  let actives, decoys =
    let train_mols = L.map mol_of_line training_set in
    L.partition FpMol.is_active train_mols in
  let n_acts = L.length actives in
  let n_decs = L.length decoys in
  let n = L.length validation_set in
  let res =
    L.parmapi ~pin_cores:true ncores (fun i valid_mol_line ->
        let valid_mol = mol_of_line valid_mol_line in
        if i mod 100 = 0 then printf "processed: %d/%d\r%!" i n;
        Indexed_mol.create valid_mol actives n_acts decoys n_decs
      ) validation_set in
  printf "processed: %d/%d\n%!" n n;
  res

(* find a good bandwidth using a global optimization heuristic *)
let bandwidth_mine_heuristic
    max_evals kernel ncores training_set validation_set =
  let indexed_mols = index_molecules ncores training_set validation_set in
  (* ncores=1 because PARALLELIZATION IN THERE DOES NOT ACCELERATE
     eval_solution_indexed's granularity is too fine/efficient *)
  optimize_global_bandwidth max_evals 1 kernel indexed_mols

let bandwidth_mine_brute_priv
    nsteps kernel ncores training_set validation_set =
  let indexed_mols = index_molecules ncores training_set validation_set in
  let lambdas = L.frange 0.0 `To 1.0 nsteps in
  Parany.Parmap.parmap ncores (fun lambda ->
      let auc = eval_solution_indexed_brute kernel indexed_mols lambda in
      (lambda, auc)
    ) lambdas

(* find a good bandwidth using brute force *)
let bandwidth_mine_brute nsteps kernel ncores training_set validation_set =
  let lambda_aucs =
    bandwidth_mine_brute_priv
      nsteps kernel ncores training_set validation_set in
  if !Flags.verbose then
    L.iter (fun (lambda, auc) ->
        Log.info "brute: %f %.3f" lambda auc
      ) lambda_aucs;
  L.maximum (fun (_lambda1, auc1) (_lambda2, auc2) ->
      BatFloat.compare auc1 auc2
    ) lambda_aucs

(* find a good bandwidth using NxCV.
   Since the optimizer doesn't explore all values of lambda, we cannot use it
   during several folds cross validation; hence we use brute force *)
let bandwidth_mine_nfolds nsteps kernel ncores train_valid_set nfolds =
  let folds = L.cv_folds nfolds train_valid_set in
  (* compute AUC as a function of lambda on each validation fold *)
  let lambda_aucs =
    L.mapi (fun i (train, valid) ->
        Log.info "fold: %d" i;
        bandwidth_mine_brute_priv nsteps kernel ncores train valid
      ) folds in
  (* gather results *)
  let init =
    let lambdas = L.frange 0.0 `To 1.0 nsteps in
    (* init map with all possible lambdas, for simplicity *)
    L.fold_left (fun acc lambda ->
        FloatMap.add lambda [] acc
      ) FloatMap.empty lambdas in
  let lambda2aucs =
    L.fold_left (fun acc1 one_fold_lambda_aucs ->
        L.fold_left (fun acc2 (lambda, auc) ->
            FloatMap.modify lambda (fun prev_aucs -> auc :: prev_aucs) acc2
          ) acc1 one_fold_lambda_aucs
      ) init lambda_aucs in
  (* average and (optionally) log them *)
  let lambda_avg_aucs =
    FloatMap.fold (fun lambda aucs acc ->
        assert(L.length aucs = nfolds);
        let avg_auc = L.favg aucs in
        if !Flags.verbose then
          begin
            let buff = Buffer.create 80 in
            bprintf buff "nfolds: %f" lambda;
            L.iter (fun auc ->
                bprintf buff " %.3f" auc
              ) aucs;
            bprintf buff " %.3f" avg_auc;
            Log.info "%s" (Buffer.contents buff)
          end;
        (lambda, avg_auc) :: acc
      ) lambda2aucs [] in
  (* return the best *)
  L.maximum (fun (_lambda1, auc1) (_lambda2, auc2) ->
      BatFloat.compare auc1 auc2
    ) lambda_avg_aucs

let platt_proba_fun = function
  | None -> (fun _raw_score -> 0.0)
  | Some (a, b) -> ROC.platt_probability a b

let only_test_single maybe_ab ?(no_sort = false)
    ncores scores_fn kernel bwidth training_set test_set =
  let n_acts = ref 0 in
  let n_decs = ref 0 in
  let mols_bst =
    let train_mols = L.map mol_of_line training_set in
    let n_acts', n_decs' = L.filter_counts FpMol.is_active train_mols in
    n_acts := n_acts';
    n_decs := n_decs';
    Bstree.(create 50 Two_bands (A.of_list train_mols)) in
  let test_mols = L.map mol_of_line test_set in
  let score_labels =
      Parany.Parmap.parmap ncores (
        score_one_single kernel !n_acts !n_decs bwidth mols_bst
      ) test_mols
  in
  if no_sort then
    score_labels
  else
    (* output scores sorted from highest to lowest; so that keeping only top N
       is easy *)
    let decreasing_scores = ROC.rank_order_by_score score_labels in
    Utls.with_out_file scores_fn (fun scores_out ->
        let proba = platt_proba_fun maybe_ab in
        L.iter (fun sl ->
            let score = SL.get_score sl in
            let p = proba score in
            fprintf scores_out "%s %.3f\n" (SL.to_string sl) p
          ) decreasing_scores
      );
    decreasing_scores

let train_test_split p lines =
  assert(p >= 0.0 && p <= 1.0);
  let n = float (L.length lines) in
  let for_training = BatFloat.round_to_int (p *. n) in
  let train, test = BatList.takedrop for_training lines in
  assert(L.length train = for_training);
  (train, test)

(* find the best threshold to do classification instead of ranking;
   by maximization of MCC over the threshold's range *)
let mcc_scan score_labels =
  let smin', smax' =
    L.min_max ~cmp:(fun x y ->
        BatFloat.compare (SL.get_score x) (SL.get_score y)
      ) score_labels in
  let smin, smax =
    if !Flags.score_fun = Probability then
      (* the min proba is NaN instead of being 0.0;
       * since the proba is undefined in parts of the chemical space
       * when using vanishing kernels *)
      (0.0, 1.0)
    else
      SL.(get_score smin', get_score smax') in
  let thresholds = L.frange smin `To smax 100 in
  let mccs =
    L.map (fun t ->
        let mcc = ROC.mcc t score_labels in
        (t, mcc)
      ) thresholds in
  let (_tmin, _mcc_min), (threshold, mcc_max) =
    L.min_max ~cmp:(fun (_t1, mcc1) (_t2, mcc2) ->
        BatFloat.compare (mcc1) (mcc2)
      ) mccs in
  (threshold, mcc_max)

type capping_method =
  | Fraction of float (* only keep given fraction of the decoys *)
  | Number of int (* constrain the total number of molecules
                   * in the dataset; but keep all actives *)
  | Factor of int (* only keep decoys up to |A|*factor *)
  | No_cap (* disable capping: the default *)

let get_cap maybe_frac maybe_num maybe_fact =
  match (maybe_frac, maybe_num, maybe_fact) with
  | (None, None, None) -> No_cap
  | (Some f, None, None) -> Fraction f
  | (None, Some i, None) -> Number i
  | (None, None, Some x) -> Factor x
  | _ -> failwith
           "Common.get_cap: choose only one of {Fraction|Number|Factor}"

(* Only keep a fraction of the decoys, but retain all actives.
   Fail in case it is not possible to have at least as many decoys
   as actives. *)
let rec maybe_cap rng (cap_m: capping_method) numbered_lines =
  match cap_m with
  | No_cap ->
    let n_acts, n_decs =
      L.filter_counts
        (fun (_i, line) ->
           FpMol.mol_is_active line
        ) numbered_lines in
    Utls.enforce (n_decs >= n_acts) "Bwmine: not enough decoys";
    L.shuffle ~state:rng numbered_lines
  | Number n ->
    let n_acts, n_decs =
      L.filter_counts
        (fun (_i, line) ->
           FpMol.mol_is_active line
        ) numbered_lines in
    let retain_decs = n - n_acts in
    Utls.enforce (retain_decs >= n_acts) "Bwmine: not enough decoys";
    (* transform it into a fraction *)
    let f = (float retain_decs) /. (float n_decs) in
    if f >= 1.0 then
      maybe_cap rng No_cap numbered_lines
    else
      maybe_cap rng (Fraction f) numbered_lines
  | Factor f ->
    let actives, decoys =
      L.partition (fun (_i, line) ->
          FpMol.mol_is_active line
        ) numbered_lines in
    let n_acts = L.length actives in
    let n_decs = L.length decoys in
    let retain_decs = min (f * n_acts) n_decs in
    Utls.enforce (retain_decs >= n_acts) "Bwmine: not enough decoys";
    (* take random partition of the decoys *)
    let rand_decoys = L.shuffle ~state:rng decoys in
    let decoys_kept = L.really_take retain_decs rand_decoys in
    let to_keep = L.rev_append actives decoys_kept in
    L.shuffle ~state:rng to_keep
  | Fraction f ->
    assert(f > 0.0 && f <= 1.0);
    if f = 1.0 then
      (* no need to cap; just shuffle *)
      L.shuffle ~state:rng numbered_lines
    else
      let actives, decoys =
        L.partition (fun (_i, line) ->
            FpMol.mol_is_active line
          ) numbered_lines in
      let n_acts = L.length actives in
      let n_decs = L.length decoys in
      let retain_decs = BatFloat.round_to_int (f *. (float n_decs)) in
      Utls.enforce (retain_decs >= n_acts) "Bwmine: not enough decoys";
      Log.info "capping: |A|=%d |D|=%d to %d"
        n_acts n_decs (retain_decs + n_acts);
      (* take random partition of the decoys *)
      let rand_decoys = L.shuffle ~state:rng decoys in
      let decoys_kept = L.really_take retain_decs rand_decoys in
      let to_keep = L.rev_append actives decoys_kept in
      L.shuffle ~state:rng to_keep
