let () = Wrap_utils.init ();;
let __wrap_namespace = Py.import "sklearn.tree"

let get_py name = Py.Module.get __wrap_namespace name
module BaseDecisionTree = struct
type tag = [`BaseDecisionTree]
type t = [`BaseDecisionTree | `BaseEstimator | `MultiOutputMixin | `Object] Obj.t
let of_pyobject x = ((Obj.of_pyobject x) : t)
let to_pyobject x = Obj.to_pyobject x
let as_estimator x = (x :> [`BaseEstimator] Obj.t)
let as_multi_output x = (x :> [`MultiOutputMixin] Obj.t)
let apply ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "apply"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let cost_complexity_pruning_path ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "cost_complexity_pruning_path"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> (fun x -> ((Wrap_utils.id (Py.Tuple.get x 0)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 1)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 2))))
let decision_path ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "decision_path"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [`ArrayLike|`Object|`Spmatrix] Np.Obj.t))
let fit ?sample_weight ?check_input ?x_idx_sorted ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "fit"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", sample_weight); ("check_input", check_input); ("X_idx_sorted", x_idx_sorted); ("X", Some(x )); ("y", Some(y ))])
     |> of_pyobject
let get_depth self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_depth"
     [||]
     []

let get_n_leaves self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_n_leaves"
     [||]
     []

let get_params ?deep self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_params"
     [||]
     (Wrap_utils.keyword_args [("deep", Wrap_utils.Option.map deep Py.Bool.of_bool)])
     |> Dict.of_pyobject
let predict ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let set_params ?params self =
   Py.Module.get_function_with_keywords (to_pyobject self) "set_params"
     [||]
     (match params with None -> [] | Some x -> x)
     |> of_pyobject
let to_string self = Py.Object.to_string (to_pyobject self)
let show self = to_string self
let pp formatter self = Format.fprintf formatter "%s" (show self)

end
module DecisionTreeClassifier = struct
type tag = [`DecisionTreeClassifier]
type t = [`BaseDecisionTree | `BaseEstimator | `ClassifierMixin | `DecisionTreeClassifier | `MultiOutputMixin | `Object] Obj.t
let of_pyobject x = ((Obj.of_pyobject x) : t)
let to_pyobject x = Obj.to_pyobject x
let as_classifier x = (x :> [`ClassifierMixin] Obj.t)
let as_estimator x = (x :> [`BaseEstimator] Obj.t)
let as_decision_tree x = (x :> [`BaseDecisionTree] Obj.t)
let as_multi_output x = (x :> [`MultiOutputMixin] Obj.t)
                  let create ?criterion ?splitter ?max_depth ?min_samples_split ?min_samples_leaf ?min_weight_fraction_leaf ?max_features ?random_state ?max_leaf_nodes ?min_impurity_decrease ?min_impurity_split ?class_weight ?presort ?ccp_alpha () =
                     Py.Module.get_function_with_keywords __wrap_namespace "DecisionTreeClassifier"
                       [||]
                       (Wrap_utils.keyword_args [("criterion", Wrap_utils.Option.map criterion (function
| `Gini -> Py.String.of_string "gini"
| `Entropy -> Py.String.of_string "entropy"
)); ("splitter", Wrap_utils.Option.map splitter (function
| `Best -> Py.String.of_string "best"
| `Random -> Py.String.of_string "random"
)); ("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("min_samples_split", Wrap_utils.Option.map min_samples_split (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_samples_leaf", Wrap_utils.Option.map min_samples_leaf (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_weight_fraction_leaf", Wrap_utils.Option.map min_weight_fraction_leaf Py.Float.of_float); ("max_features", Wrap_utils.Option.map max_features (function
| `Auto -> Py.String.of_string "auto"
| `Log2 -> Py.String.of_string "log2"
| `F x -> Py.Float.of_float x
| `Sqrt -> Py.String.of_string "sqrt"
| `I x -> Py.Int.of_int x
)); ("random_state", Wrap_utils.Option.map random_state Py.Int.of_int); ("max_leaf_nodes", Wrap_utils.Option.map max_leaf_nodes Py.Int.of_int); ("min_impurity_decrease", Wrap_utils.Option.map min_impurity_decrease Py.Float.of_float); ("min_impurity_split", Wrap_utils.Option.map min_impurity_split Py.Float.of_float); ("class_weight", Wrap_utils.Option.map class_weight (function
| `List_of_dict x -> Wrap_utils.id x
| `Balanced -> Py.String.of_string "balanced"
| `DictIntToFloat x -> (Py.Dict.of_bindings_map Py.Int.of_int Py.Float.of_float) x
)); ("presort", presort); ("ccp_alpha", Wrap_utils.Option.map ccp_alpha Py.Float.of_float)])
                       |> of_pyobject
let apply ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "apply"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let cost_complexity_pruning_path ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "cost_complexity_pruning_path"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> (fun x -> ((Wrap_utils.id (Py.Tuple.get x 0)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 1)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 2))))
let decision_path ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "decision_path"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [`ArrayLike|`Object|`Spmatrix] Np.Obj.t))
let fit ?sample_weight ?check_input ?x_idx_sorted ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "fit"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X_idx_sorted", Wrap_utils.Option.map x_idx_sorted Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> of_pyobject
let get_depth self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_depth"
     [||]
     []

let get_n_leaves self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_n_leaves"
     [||]
     []

let get_params ?deep self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_params"
     [||]
     (Wrap_utils.keyword_args [("deep", Wrap_utils.Option.map deep Py.Bool.of_bool)])
     |> Dict.of_pyobject
let predict ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let predict_log_proba ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict_log_proba"
     [||]
     (Wrap_utils.keyword_args [("X", Some(x |> Np.Obj.to_pyobject))])

let predict_proba ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict_proba"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let score ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "score"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> Py.Float.to_float
let set_params ?params self =
   Py.Module.get_function_with_keywords (to_pyobject self) "set_params"
     [||]
     (match params with None -> [] | Some x -> x)
     |> of_pyobject

let classes_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "classes_" with
  | None -> failwith "attribute classes_ not found"
  | Some x -> if Py.is_none x then None else Some ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) x)

let classes_ self = match classes_opt self with
  | None -> raise Not_found
  | Some x -> x

let feature_importances_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "feature_importances_" with
  | None -> failwith "attribute feature_importances_ not found"
  | Some x -> if Py.is_none x then None else Some ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) x)

let feature_importances_ self = match feature_importances_opt self with
  | None -> raise Not_found
  | Some x -> x

let warning_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "Warning" with
  | None -> failwith "attribute Warning not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let warning self = match warning_opt self with
  | None -> raise Not_found
  | Some x -> x

let max_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "max_features_" with
  | None -> failwith "attribute max_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let max_features_ self = match max_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_classes_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_classes_" with
  | None -> failwith "attribute n_classes_ not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let n_classes_ self = match n_classes_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_features_" with
  | None -> failwith "attribute n_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_features_ self = match n_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_outputs_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_outputs_" with
  | None -> failwith "attribute n_outputs_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_outputs_ self = match n_outputs_opt self with
  | None -> raise Not_found
  | Some x -> x

let tree_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "tree_" with
  | None -> failwith "attribute tree_ not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let tree_ self = match tree_opt self with
  | None -> raise Not_found
  | Some x -> x
let to_string self = Py.Object.to_string (to_pyobject self)
let show self = to_string self
let pp formatter self = Format.fprintf formatter "%s" (show self)

end
module DecisionTreeRegressor = struct
type tag = [`DecisionTreeRegressor]
type t = [`BaseDecisionTree | `BaseEstimator | `DecisionTreeRegressor | `MultiOutputMixin | `Object | `RegressorMixin] Obj.t
let of_pyobject x = ((Obj.of_pyobject x) : t)
let to_pyobject x = Obj.to_pyobject x
let as_estimator x = (x :> [`BaseEstimator] Obj.t)
let as_decision_tree x = (x :> [`BaseDecisionTree] Obj.t)
let as_regressor x = (x :> [`RegressorMixin] Obj.t)
let as_multi_output x = (x :> [`MultiOutputMixin] Obj.t)
                  let create ?criterion ?splitter ?max_depth ?min_samples_split ?min_samples_leaf ?min_weight_fraction_leaf ?max_features ?random_state ?max_leaf_nodes ?min_impurity_decrease ?min_impurity_split ?presort ?ccp_alpha () =
                     Py.Module.get_function_with_keywords __wrap_namespace "DecisionTreeRegressor"
                       [||]
                       (Wrap_utils.keyword_args [("criterion", Wrap_utils.Option.map criterion (function
| `Mse -> Py.String.of_string "mse"
| `Friedman_mse -> Py.String.of_string "friedman_mse"
| `Mae -> Py.String.of_string "mae"
)); ("splitter", Wrap_utils.Option.map splitter (function
| `Best -> Py.String.of_string "best"
| `Random -> Py.String.of_string "random"
)); ("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("min_samples_split", Wrap_utils.Option.map min_samples_split (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_samples_leaf", Wrap_utils.Option.map min_samples_leaf (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_weight_fraction_leaf", Wrap_utils.Option.map min_weight_fraction_leaf Py.Float.of_float); ("max_features", Wrap_utils.Option.map max_features (function
| `Auto -> Py.String.of_string "auto"
| `Log2 -> Py.String.of_string "log2"
| `F x -> Py.Float.of_float x
| `Sqrt -> Py.String.of_string "sqrt"
| `I x -> Py.Int.of_int x
)); ("random_state", Wrap_utils.Option.map random_state Py.Int.of_int); ("max_leaf_nodes", Wrap_utils.Option.map max_leaf_nodes Py.Int.of_int); ("min_impurity_decrease", Wrap_utils.Option.map min_impurity_decrease Py.Float.of_float); ("min_impurity_split", Wrap_utils.Option.map min_impurity_split Py.Float.of_float); ("presort", presort); ("ccp_alpha", Wrap_utils.Option.map ccp_alpha Py.Float.of_float)])
                       |> of_pyobject
let apply ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "apply"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let cost_complexity_pruning_path ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "cost_complexity_pruning_path"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> (fun x -> ((Wrap_utils.id (Py.Tuple.get x 0)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 1)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 2))))
let decision_path ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "decision_path"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [`ArrayLike|`Object|`Spmatrix] Np.Obj.t))
let fit ?sample_weight ?check_input ?x_idx_sorted ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "fit"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X_idx_sorted", Wrap_utils.Option.map x_idx_sorted Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> of_pyobject
let get_depth self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_depth"
     [||]
     []

let get_n_leaves self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_n_leaves"
     [||]
     []

let get_params ?deep self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_params"
     [||]
     (Wrap_utils.keyword_args [("deep", Wrap_utils.Option.map deep Py.Bool.of_bool)])
     |> Dict.of_pyobject
let predict ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let score ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "score"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> Py.Float.to_float
let set_params ?params self =
   Py.Module.get_function_with_keywords (to_pyobject self) "set_params"
     [||]
     (match params with None -> [] | Some x -> x)
     |> of_pyobject

let feature_importances_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "feature_importances_" with
  | None -> failwith "attribute feature_importances_ not found"
  | Some x -> if Py.is_none x then None else Some ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) x)

let feature_importances_ self = match feature_importances_opt self with
  | None -> raise Not_found
  | Some x -> x

let warning_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "Warning" with
  | None -> failwith "attribute Warning not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let warning self = match warning_opt self with
  | None -> raise Not_found
  | Some x -> x

let max_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "max_features_" with
  | None -> failwith "attribute max_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let max_features_ self = match max_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_features_" with
  | None -> failwith "attribute n_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_features_ self = match n_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_outputs_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_outputs_" with
  | None -> failwith "attribute n_outputs_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_outputs_ self = match n_outputs_opt self with
  | None -> raise Not_found
  | Some x -> x

let tree_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "tree_" with
  | None -> failwith "attribute tree_ not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let tree_ self = match tree_opt self with
  | None -> raise Not_found
  | Some x -> x
let to_string self = Py.Object.to_string (to_pyobject self)
let show self = to_string self
let pp formatter self = Format.fprintf formatter "%s" (show self)

end
module ExtraTreeClassifier = struct
type tag = [`ExtraTreeClassifier]
type t = [`BaseDecisionTree | `BaseEstimator | `ClassifierMixin | `DecisionTreeClassifier | `ExtraTreeClassifier | `MultiOutputMixin | `Object] Obj.t
let of_pyobject x = ((Obj.of_pyobject x) : t)
let to_pyobject x = Obj.to_pyobject x
let as_multi_output x = (x :> [`MultiOutputMixin] Obj.t)
let as_decision_tree x = (x :> [`BaseDecisionTree] Obj.t)
let as_decision_tree_classifier x = (x :> [`DecisionTreeClassifier] Obj.t)
let as_estimator x = (x :> [`BaseEstimator] Obj.t)
let as_classifier x = (x :> [`ClassifierMixin] Obj.t)
                  let create ?criterion ?splitter ?max_depth ?min_samples_split ?min_samples_leaf ?min_weight_fraction_leaf ?max_features ?random_state ?max_leaf_nodes ?min_impurity_decrease ?min_impurity_split ?class_weight ?ccp_alpha () =
                     Py.Module.get_function_with_keywords __wrap_namespace "ExtraTreeClassifier"
                       [||]
                       (Wrap_utils.keyword_args [("criterion", Wrap_utils.Option.map criterion (function
| `Gini -> Py.String.of_string "gini"
| `Entropy -> Py.String.of_string "entropy"
)); ("splitter", Wrap_utils.Option.map splitter (function
| `Random -> Py.String.of_string "random"
| `Best -> Py.String.of_string "best"
)); ("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("min_samples_split", Wrap_utils.Option.map min_samples_split (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_samples_leaf", Wrap_utils.Option.map min_samples_leaf (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_weight_fraction_leaf", Wrap_utils.Option.map min_weight_fraction_leaf Py.Float.of_float); ("max_features", Wrap_utils.Option.map max_features (function
| `F x -> Py.Float.of_float x
| `Sqrt -> Py.String.of_string "sqrt"
| `I x -> Py.Int.of_int x
| `PyObject x -> Wrap_utils.id x
| `None -> Py.none
)); ("random_state", Wrap_utils.Option.map random_state Py.Int.of_int); ("max_leaf_nodes", Wrap_utils.Option.map max_leaf_nodes Py.Int.of_int); ("min_impurity_decrease", Wrap_utils.Option.map min_impurity_decrease Py.Float.of_float); ("min_impurity_split", Wrap_utils.Option.map min_impurity_split Py.Float.of_float); ("class_weight", Wrap_utils.Option.map class_weight (function
| `List_of_dict x -> Wrap_utils.id x
| `Balanced -> Py.String.of_string "balanced"
| `DictIntToFloat x -> (Py.Dict.of_bindings_map Py.Int.of_int Py.Float.of_float) x
)); ("ccp_alpha", Wrap_utils.Option.map ccp_alpha Py.Float.of_float)])
                       |> of_pyobject
let apply ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "apply"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let cost_complexity_pruning_path ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "cost_complexity_pruning_path"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> (fun x -> ((Wrap_utils.id (Py.Tuple.get x 0)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 1)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 2))))
let decision_path ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "decision_path"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [`ArrayLike|`Object|`Spmatrix] Np.Obj.t))
let fit ?sample_weight ?check_input ?x_idx_sorted ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "fit"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X_idx_sorted", Wrap_utils.Option.map x_idx_sorted Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> of_pyobject
let get_depth self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_depth"
     [||]
     []

let get_n_leaves self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_n_leaves"
     [||]
     []

let get_params ?deep self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_params"
     [||]
     (Wrap_utils.keyword_args [("deep", Wrap_utils.Option.map deep Py.Bool.of_bool)])
     |> Dict.of_pyobject
let predict ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let predict_log_proba ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict_log_proba"
     [||]
     (Wrap_utils.keyword_args [("X", Some(x |> Np.Obj.to_pyobject))])

let predict_proba ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict_proba"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let score ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "score"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> Py.Float.to_float
let set_params ?params self =
   Py.Module.get_function_with_keywords (to_pyobject self) "set_params"
     [||]
     (match params with None -> [] | Some x -> x)
     |> of_pyobject

let classes_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "classes_" with
  | None -> failwith "attribute classes_ not found"
  | Some x -> if Py.is_none x then None else Some ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) x)

let classes_ self = match classes_opt self with
  | None -> raise Not_found
  | Some x -> x

let max_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "max_features_" with
  | None -> failwith "attribute max_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let max_features_ self = match max_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_classes_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_classes_" with
  | None -> failwith "attribute n_classes_ not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let n_classes_ self = match n_classes_opt self with
  | None -> raise Not_found
  | Some x -> x

let feature_importances_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "feature_importances_" with
  | None -> failwith "attribute feature_importances_ not found"
  | Some x -> if Py.is_none x then None else Some ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) x)

let feature_importances_ self = match feature_importances_opt self with
  | None -> raise Not_found
  | Some x -> x

let warning_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "Warning" with
  | None -> failwith "attribute Warning not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let warning self = match warning_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_features_" with
  | None -> failwith "attribute n_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_features_ self = match n_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_outputs_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_outputs_" with
  | None -> failwith "attribute n_outputs_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_outputs_ self = match n_outputs_opt self with
  | None -> raise Not_found
  | Some x -> x

let tree_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "tree_" with
  | None -> failwith "attribute tree_ not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let tree_ self = match tree_opt self with
  | None -> raise Not_found
  | Some x -> x
let to_string self = Py.Object.to_string (to_pyobject self)
let show self = to_string self
let pp formatter self = Format.fprintf formatter "%s" (show self)

end
module ExtraTreeRegressor = struct
type tag = [`ExtraTreeRegressor]
type t = [`BaseDecisionTree | `BaseEstimator | `DecisionTreeRegressor | `ExtraTreeRegressor | `MultiOutputMixin | `Object | `RegressorMixin] Obj.t
let of_pyobject x = ((Obj.of_pyobject x) : t)
let to_pyobject x = Obj.to_pyobject x
let as_multi_output x = (x :> [`MultiOutputMixin] Obj.t)
let as_decision_tree x = (x :> [`BaseDecisionTree] Obj.t)
let as_regressor x = (x :> [`RegressorMixin] Obj.t)
let as_estimator x = (x :> [`BaseEstimator] Obj.t)
let as_decision_tree_regressor x = (x :> [`DecisionTreeRegressor] Obj.t)
                  let create ?criterion ?splitter ?max_depth ?min_samples_split ?min_samples_leaf ?min_weight_fraction_leaf ?max_features ?random_state ?min_impurity_decrease ?min_impurity_split ?max_leaf_nodes ?ccp_alpha () =
                     Py.Module.get_function_with_keywords __wrap_namespace "ExtraTreeRegressor"
                       [||]
                       (Wrap_utils.keyword_args [("criterion", Wrap_utils.Option.map criterion (function
| `Mse -> Py.String.of_string "mse"
| `Friedman_mse -> Py.String.of_string "friedman_mse"
| `Mae -> Py.String.of_string "mae"
)); ("splitter", Wrap_utils.Option.map splitter (function
| `Random -> Py.String.of_string "random"
| `Best -> Py.String.of_string "best"
)); ("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("min_samples_split", Wrap_utils.Option.map min_samples_split (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_samples_leaf", Wrap_utils.Option.map min_samples_leaf (function
| `I x -> Py.Int.of_int x
| `F x -> Py.Float.of_float x
)); ("min_weight_fraction_leaf", Wrap_utils.Option.map min_weight_fraction_leaf Py.Float.of_float); ("max_features", Wrap_utils.Option.map max_features (function
| `F x -> Py.Float.of_float x
| `Sqrt -> Py.String.of_string "sqrt"
| `I x -> Py.Int.of_int x
| `PyObject x -> Wrap_utils.id x
| `None -> Py.none
)); ("random_state", Wrap_utils.Option.map random_state Py.Int.of_int); ("min_impurity_decrease", Wrap_utils.Option.map min_impurity_decrease Py.Float.of_float); ("min_impurity_split", Wrap_utils.Option.map min_impurity_split Py.Float.of_float); ("max_leaf_nodes", Wrap_utils.Option.map max_leaf_nodes Py.Int.of_int); ("ccp_alpha", Wrap_utils.Option.map ccp_alpha Py.Float.of_float)])
                       |> of_pyobject
let apply ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "apply"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let cost_complexity_pruning_path ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "cost_complexity_pruning_path"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> (fun x -> ((Wrap_utils.id (Py.Tuple.get x 0)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 1)), ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) (Py.Tuple.get x 2))))
let decision_path ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "decision_path"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [`ArrayLike|`Object|`Spmatrix] Np.Obj.t))
let fit ?sample_weight ?check_input ?x_idx_sorted ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "fit"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X_idx_sorted", Wrap_utils.Option.map x_idx_sorted Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> of_pyobject
let get_depth self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_depth"
     [||]
     []

let get_n_leaves self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_n_leaves"
     [||]
     []

let get_params ?deep self =
   Py.Module.get_function_with_keywords (to_pyobject self) "get_params"
     [||]
     (Wrap_utils.keyword_args [("deep", Wrap_utils.Option.map deep Py.Bool.of_bool)])
     |> Dict.of_pyobject
let predict ?check_input ~x self =
   Py.Module.get_function_with_keywords (to_pyobject self) "predict"
     [||]
     (Wrap_utils.keyword_args [("check_input", Wrap_utils.Option.map check_input Py.Bool.of_bool); ("X", Some(x |> Np.Obj.to_pyobject))])
     |> (fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t))
let score ?sample_weight ~x ~y self =
   Py.Module.get_function_with_keywords (to_pyobject self) "score"
     [||]
     (Wrap_utils.keyword_args [("sample_weight", Wrap_utils.Option.map sample_weight Np.Obj.to_pyobject); ("X", Some(x |> Np.Obj.to_pyobject)); ("y", Some(y |> Np.Obj.to_pyobject))])
     |> Py.Float.to_float
let set_params ?params self =
   Py.Module.get_function_with_keywords (to_pyobject self) "set_params"
     [||]
     (match params with None -> [] | Some x -> x)
     |> of_pyobject

let max_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "max_features_" with
  | None -> failwith "attribute max_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let max_features_ self = match max_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_features_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_features_" with
  | None -> failwith "attribute n_features_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_features_ self = match n_features_opt self with
  | None -> raise Not_found
  | Some x -> x

let feature_importances_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "feature_importances_" with
  | None -> failwith "attribute feature_importances_ not found"
  | Some x -> if Py.is_none x then None else Some ((fun py -> (Np.Obj.of_pyobject py : [>`ArrayLike] Np.Obj.t)) x)

let feature_importances_ self = match feature_importances_opt self with
  | None -> raise Not_found
  | Some x -> x

let warning_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "Warning" with
  | None -> failwith "attribute Warning not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let warning self = match warning_opt self with
  | None -> raise Not_found
  | Some x -> x

let n_outputs_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "n_outputs_" with
  | None -> failwith "attribute n_outputs_ not found"
  | Some x -> if Py.is_none x then None else Some (Py.Int.to_int x)

let n_outputs_ self = match n_outputs_opt self with
  | None -> raise Not_found
  | Some x -> x

let tree_opt self =
  match Py.Object.get_attr_string (to_pyobject self) "tree_" with
  | None -> failwith "attribute tree_ not found"
  | Some x -> if Py.is_none x then None else Some (Wrap_utils.id x)

let tree_ self = match tree_opt self with
  | None -> raise Not_found
  | Some x -> x
let to_string self = Py.Object.to_string (to_pyobject self)
let show self = to_string self
let pp formatter self = Format.fprintf formatter "%s" (show self)

end
                  let export_graphviz ?out_file ?max_depth ?feature_names ?class_names ?label ?filled ?leaves_parallel ?impurity ?node_ids ?proportion ?rotate ?rounded ?special_characters ?precision ~decision_tree () =
                     Py.Module.get_function_with_keywords __wrap_namespace "export_graphviz"
                       [||]
                       (Wrap_utils.keyword_args [("out_file", Wrap_utils.Option.map out_file (function
| `S x -> Py.String.of_string x
| `File_object x -> Wrap_utils.id x
)); ("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("feature_names", Wrap_utils.Option.map feature_names (Py.List.of_list_map Py.String.of_string)); ("class_names", Wrap_utils.Option.map class_names (function
| `StringList x -> (Py.List.of_list_map Py.String.of_string) x
| `Bool x -> Py.Bool.of_bool x
)); ("label", Wrap_utils.Option.map label (function
| `All -> Py.String.of_string "all"
| `Root -> Py.String.of_string "root"
| `None -> Py.String.of_string "none"
)); ("filled", Wrap_utils.Option.map filled Py.Bool.of_bool); ("leaves_parallel", Wrap_utils.Option.map leaves_parallel Py.Bool.of_bool); ("impurity", Wrap_utils.Option.map impurity Py.Bool.of_bool); ("node_ids", Wrap_utils.Option.map node_ids Py.Bool.of_bool); ("proportion", Wrap_utils.Option.map proportion Py.Bool.of_bool); ("rotate", Wrap_utils.Option.map rotate Py.Bool.of_bool); ("rounded", Wrap_utils.Option.map rounded Py.Bool.of_bool); ("special_characters", Wrap_utils.Option.map special_characters Py.Bool.of_bool); ("precision", Wrap_utils.Option.map precision Py.Int.of_int); ("decision_tree", Some(decision_tree |> Np.Obj.to_pyobject))])
                       |> (fun py -> if Py.is_none py then None else Some (Py.String.to_string py))
let export_text ?feature_names ?max_depth ?spacing ?decimals ?show_weights ~decision_tree () =
   Py.Module.get_function_with_keywords __wrap_namespace "export_text"
     [||]
     (Wrap_utils.keyword_args [("feature_names", Wrap_utils.Option.map feature_names (fun ml -> Py.List.of_list_map Py.String.of_string ml)); ("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("spacing", Wrap_utils.Option.map spacing Py.Int.of_int); ("decimals", Wrap_utils.Option.map decimals Py.Int.of_int); ("show_weights", Wrap_utils.Option.map show_weights Py.Bool.of_bool); ("decision_tree", Some(decision_tree |> Np.Obj.to_pyobject))])
     |> Py.String.to_string
                  let plot_tree ?max_depth ?feature_names ?class_names ?label ?filled ?impurity ?node_ids ?proportion ?rotate ?rounded ?precision ?ax ?fontsize ~decision_tree () =
                     Py.Module.get_function_with_keywords __wrap_namespace "plot_tree"
                       [||]
                       (Wrap_utils.keyword_args [("max_depth", Wrap_utils.Option.map max_depth Py.Int.of_int); ("feature_names", Wrap_utils.Option.map feature_names (Py.List.of_list_map Py.String.of_string)); ("class_names", Wrap_utils.Option.map class_names (function
| `StringList x -> (Py.List.of_list_map Py.String.of_string) x
| `Bool x -> Py.Bool.of_bool x
)); ("label", Wrap_utils.Option.map label (function
| `All -> Py.String.of_string "all"
| `Root -> Py.String.of_string "root"
| `None -> Py.String.of_string "none"
)); ("filled", Wrap_utils.Option.map filled Py.Bool.of_bool); ("impurity", Wrap_utils.Option.map impurity Py.Bool.of_bool); ("node_ids", Wrap_utils.Option.map node_ids Py.Bool.of_bool); ("proportion", Wrap_utils.Option.map proportion Py.Bool.of_bool); ("rotate", Wrap_utils.Option.map rotate Py.Bool.of_bool); ("rounded", Wrap_utils.Option.map rounded Py.Bool.of_bool); ("precision", Wrap_utils.Option.map precision Py.Int.of_int); ("ax", ax); ("fontsize", Wrap_utils.Option.map fontsize Py.Int.of_int); ("decision_tree", Some(decision_tree |> Np.Obj.to_pyobject))])

