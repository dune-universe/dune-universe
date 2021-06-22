(* Copyright (C) 2021, Francois Berenger

   Tsuda laboratory, Tokyo university,
   5-1-5 Kashiwa-no-ha, Kashiwa-shi, Chiba-ken, 277-8561, Japan. *)

(* Random Forests Regressor *)

module A = BatArray
module IntMap = BatMap.Int
module IntSet = BatSet.Int
module L = BatList
module Log = Dolog.Log
module RNG = Random.State
module Ht = BatHashtbl

type features = int IntMap.t
type dep_var = float

type sample = features (* X *) *
              dep_var (* y *)

type tree = Leaf of dep_var
          | Node of tree (* lhs *) *
                    int * int (* (feature, threshold) *) *
                    tree (* rhs *)

type metric = MSE (* Mean Squared Error *)
            | MAE (* Mean Absolute Error *)
            | MAD (* Median Absolute Deviation *)

let square x =
  x *. x

let average_dep_vars samples =
  let n = float (A.length samples) in
  let sum =
    A.fold (fun acc (_features, value) ->
        acc +. value
      ) 0.0 samples in
  sum /. n

let mean_squared_error samples =
  let n = float (A.length samples) in
  let avg = average_dep_vars samples in
  let sum_squared_errors =
    A.fold (fun acc (_sample, y) ->
        acc +. square (y -. avg)
      ) 0.0 samples in
  sum_squared_errors /. n

let mean_absolute_error samples =
  let n = float (A.length samples) in
  let avg = average_dep_vars samples in
  let sum_abs_errors =
    A.fold (fun acc (_sample, y) ->
        acc +. abs_float (y -. avg)
      ) 0.0 samples in
  sum_abs_errors /. n

let mean_absolute_deviation samples =
  let n = float (A.length samples) in
  let values = A.map snd samples in
  let med = Utls.array_medianf values in
  let abs_devs = A.map (fun x -> abs_float (x -. med)) values in
  (Utls.array_medianf abs_devs) /. n

let metric_of = function
  | MSE -> mean_squared_error
  | MAE -> mean_absolute_error
  | MAD -> mean_absolute_deviation

(* maybe this is called the "Classification And Regression Tree" (CART)
   algorithm in the litterature *)
let tree_grow (rng: Random.State.t) (* seeded RNG *)
    (metric: sample array -> float) (* hyper params *)
    (max_features: int)
    (max_samples: int)
    (min_node_size: int)
    (training_set: sample array) (* dataset *) : tree * int array =
  let bootstrap, oob =
    (* First randomization introduced by random forests: bootstrap sampling *)
    Utls.array_bootstrap_sample_OOB rng max_samples training_set in
  let rec loop samples =
    (* min_node_size is a regularization parameter; it also allows to
     * abort tree building (might be interesting for very large datasets) *)
    if A.length samples <= min_node_size then
      Leaf (average_dep_vars samples)
    else
      (* collect all non constant features *)
      let split_candidates =
        let all_candidates = RFC.collect_non_constant_features samples in
        (* randomly keep only N of them:
           Second randomization introduced by random forests
           (random feature sampling). *)
        L.take max_features (L.shuffle ~state:rng all_candidates) in
      match split_candidates with
      | [] -> (* cannot discriminate samples further *)
        Leaf (average_dep_vars samples)
      | _ ->
        (* select the (feature, threshold) pair minimizing cost *)
        let candidate_splits =
          L.fold (fun acc1 (feature, values) ->
              IntSet.fold (fun value acc2 ->
                  (feature, value,
                   RFC.partition_samples feature value samples)
                  :: acc2
                ) values acc1
            ) [] split_candidates in
        let split_costs =
          L.rev_map (fun (feature, value, (left, right)) ->
              let cost = RFC.cost_function metric left right in
              (cost, feature, value, (left, right))
            ) candidate_splits in
        (* choose one split minimizing cost *)
        let cost, feature, threshold, (left, right) =
          RFC.choose_min_cost rng split_costs in
        if A.length left = 0 then
          Leaf (average_dep_vars right)
        else if A.length right = 0 then
          Leaf (average_dep_vars left)
        else if cost = 0.0 then
          (* if the cost is minimal: pure nodes -> stop digging *)
          Node (Leaf (average_dep_vars left), feature, threshold,
                Leaf (average_dep_vars right))
        else
          Node (loop left, feature, threshold, loop right)
  in
  (loop (* 0 *) bootstrap, oob)

(* array of all samples whose index is listed *)
let extract indexes (samples: sample array): sample array =
  A.map (A.unsafe_get samples) indexes

let rand_max_bound = 1073741823 (* 2^30 - 1 *)

let forest_grow
    ncores rng metric ntrees max_features max_samples min_node_size train =
  (* treat the RNG as a seed stream, for reproducibility
     despite potentially out of order parallel run *)
  let seeds = A.init ntrees (fun _ -> RNG.int rng rand_max_bound) in
  RFC.array_parmap ncores
    (fun seed ->
       let rng' = RNG.make [|seed|] in
       tree_grow rng' metric max_features max_samples min_node_size train
    )
    seeds (Leaf 0.0, [||])

type forest = (tree * int array) array

(* before saving a model, we might want to just get rid of the OOB
 * sample indexes *)
let drop_OOB (f: forest): forest =
  A.map (fun (t, _oob) -> (t, [||])) f

let train (ncores: int)
    (rng: Random.State.t)
    (metric: metric)
    (ntrees: int)
    (max_features: RFC.int_or_float)
    (card_features: int)
    (max_samples: RFC.int_or_float)
    (min_node_size: int)
    (train: sample array): forest =
  Utls.enforce (1 <= ntrees) "RFC.train: ntrees < 1";
  let metric_f = metric_of metric in
  let max_feats =
    RFC.ratio_to_int 1 card_features "max_features" max_features in
  let n = A.length train in
  let max_samps =
    RFC.ratio_to_int 1 n "max_samples" max_samples in
  let min_node =
    let () =
      Utls.enforce (1 <= min_node_size && min_node_size < n)
        "RFC.train: min_node_size not in [1,n[" in
    min_node_size in
  forest_grow
    ncores rng metric_f ntrees max_feats max_samps min_node train

(* predict for one sample using one tree *)
let tree_predict tree (features, _dep_var) =
  let rec loop = function
    | Leaf dep_var -> dep_var
    | Node (lhs, feature, threshold, rhs) ->
      let value = IntMap.find_default 0 feature features in
      if value <= threshold then
        loop lhs
      else
        loop rhs in
  loop tree

(* predict an average value and its standard deviation
 * over all trees in the forest *)
let predict_one ncores forest x =
  let pred_vals =
    RFC.array_parmap ncores
      (fun (tree, _oob) -> tree_predict tree x) forest 0.0 in
  let avg = A.favg pred_vals in
  let std = Utls.std_dev avg pred_vals in
  (avg, std)

(* will scale better than predict_one *)
let predict_many ncores forest xs =
  RFC.array_parmap ncores (predict_one 1 forest) xs (0.0, 0.0)

let predict_OOB forest train =
  let n = A.length train in
  let oob_idx2preds = Ht.create n in
  A.iter (fun (tree, oob) ->
      let train_OOB = extract oob train in
      let truths = A.map snd train_OOB in
      let preds = A.map (tree_predict tree) train_OOB in
      Utls.array_iter3 oob truths preds (fun oob_idx truth pred ->
          try
            let prev_truth, prev_preds = Ht.find oob_idx2preds oob_idx in
            assert(prev_truth = truth);
            Ht.replace oob_idx2preds oob_idx (prev_truth, pred :: prev_preds)
          with Not_found ->
            Ht.add oob_idx2preds oob_idx (truth, [pred])
        )
    ) forest;
  let truth_preds = A.create (Ht.length oob_idx2preds) (0.0, 0.0) in
  Utls.ht_iteri (fun i _oob_idx (truth, preds) ->
      A.unsafe_set truth_preds i (truth, L.favg preds)
    ) oob_idx2preds;
  truth_preds

type filename = string

let save fn forest =
  Utls.save fn (drop_OOB forest)

let restore fn =
  Utls.restore fn

(* coefficient of determination R2 *)
let r2 truth_preds =
  let n = float (A.length truth_preds) in
  let sum_squared_diffs =
    A.fold_left (fun acc (x, y) ->
        acc +. Utls.square (x -. y)
      ) 0.0 truth_preds in
  let sum_squared_truth_diffs =
    let avg_truth =
      let sum =
        A.fold_left (fun acc (truth, _pred) ->
            acc +. truth
          ) 0.0 truth_preds in
      sum /. n in
    A.fold_left (fun acc (truth, _pred) ->
        acc +. square (truth -. avg_truth)
      ) 0.0 truth_preds in
  1.0 -. (sum_squared_diffs /. sum_squared_truth_diffs)
