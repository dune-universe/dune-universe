
module IntMap = BatMap.Int
module IntSet = BatSet.Int

type features = int IntMap.t
type class_label = int

type sample = features (* X *) *
              class_label (* y *)

type metric = Gini (* default *)
            | Shannon (* TODO *)
            | MCC (* TODO *)

(** trained Random Forests model *)
type forest

type int_or_float = Int of int (* exact count *)
                  | Float of float (* proportion *)

(** [train ncores rng metric ntrees max_features card_features
           max_samples min_node_size training_set] *)
val train: int -> Random.State.t -> metric -> int ->
  int_or_float -> int -> int_or_float -> int -> sample array -> forest

(** [(pred_label, pred_proba) =
      predict_one ncores rng trained_forest sample] *)
val predict_one: int -> Random.State.t -> forest -> sample
  -> (class_label * float)

(** [(pred_label, pred_proba, pred_margin) =
      predict_one_margin ncores rng trained_forest sample] *)
val predict_one_margin: int -> Random.State.t -> forest -> sample
  -> (class_label * float * float)

(** like [predict_one] but for an array of samples *)
val predict_many: int -> Random.State.t -> forest -> sample array ->
  (class_label * float) array

(** like [predict_one_margin] but for an array of samples *)
val predict_many_margin: int -> Random.State.t -> forest -> sample array ->
  (class_label * float * float) array

(** use a trained forest to predict on the Out Of Bag (OOB) training set
    of each tree. The training_set must be provided in the same order
    than when the model was trained.
    Can be used to get a reliable model performance estimate,
    even if you don't have a left out test set.
    [truth_preds = predict_OOB rng forest training_set] *)
val predict_OOB: Random.State.t -> forest -> sample array ->
  (class_label * class_label) array

(** Matthews Correlation Coefficient (MCC).
    [mcc target_class_label truth_preds] *)
val mcc: class_label -> (class_label * class_label) array -> float

(** Percentage of correct prediction
    [accuracy truth_preds] *)
val accuracy: (class_label * class_label) array -> float

(** ROC AUC
    [roc_auc target_class_label preds true_labels] *)
val roc_auc: class_label -> (class_label * float) array ->
  class_label array -> float

(** make trained model forget OOB samples (reduce model size) *)
val drop_OOB: forest -> forest

type filename = string

(** Save model to file (Marshal)
    OOB samples are dropped prior to saving the model. *)
val save: filename -> forest -> unit

(** Restore model from file (Marshal) *)
val restore: filename -> forest

(** The following are needed to implement RFR *)

val collect_non_constant_features:
  (int IntMap.t * 'a) array -> (int * IntSet.t) list

val partition_samples: int -> int -> (int IntMap.t * 'a) array ->
  (int IntMap.t * 'a) array * (int IntMap.t * 'a) array

val cost_function: ('a array -> float) -> 'a array -> 'a array -> float

val choose_min_cost: Random.State.t -> (float * 'b * 'c * ('d * 'e)) list ->
  float * 'b * 'c * ('d * 'e)

(* this fun. should go into the parany library *)
val array_parmap: int -> ('a -> 'b) -> 'a array -> 'b -> 'b array

val ratio_to_int: int -> int -> string -> int_or_float -> int
