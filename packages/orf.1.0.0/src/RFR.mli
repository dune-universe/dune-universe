(* Copyright (C) 2021, Francois Berenger

   Tsuda laboratory, Tokyo university,
   5-1-5 Kashiwa-no-ha, Kashiwa-shi, Chiba-ken, 277-8561, Japan. *)

(* Random Forests Regressor *)

module IntMap = BatMap.Int

type features = int IntMap.t
type dep_var = float

type sample = features (* X *) *
              dep_var (* y *)

type tree = Leaf of dep_var
          | Node of tree (* lhs *) *
                    int * int (* (feature, threshold) *) *
                    tree (* rhs *)

type metric = MSE (* Mean Squared Error (use as default) *)
            | MAE (* Mean Absolute Error *)
            | MAD (* Median Absolute Deviation *)

type forest = (tree * int array) array

(** [train ncores rng metric ntrees max_features card_features
           max_samples min_node_size training_set] *)
val train: int -> Random.State.t -> metric -> int ->
  RFC.int_or_float -> int -> RFC.int_or_float -> int -> sample array ->
  forest

(** [(pred_avg, pred_std_dev) = predict_one ncores trained_forest sample] *)
val predict_one: int -> forest -> sample -> (dep_var * float)

(** like [predict_one] but for an array of samples *)
val predict_many: int -> forest -> sample array -> (dep_var * float) array

(** use a trained forest to predict on the Out Of Bag (OOB) training set
    of each tree. The training_set must be provided in the same order
    than when the model was trained.
    Can be used to get a reliable model performance estimate,
    even if you don't have a left out test set.
    [truth_preds = predict_OOB forest training_set] *)
val predict_OOB: forest -> sample array -> (dep_var * dep_var) array

(** [r2 truth_preds]: coefficient of determination R^2 *)
val r2: (dep_var * dep_var) array -> float

(** make trained model forget OOB samples (reduce model size) *)
val drop_OOB: forest -> forest

type filename = string

(** Save model to file (Marshal).
    OOB samples are dropped prior to saving the model. *)
val save: filename -> forest -> unit

(** Restore model from file (Marshal) *)
val restore: filename -> forest
