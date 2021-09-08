
open Printf

module A = BatArray
module L = BatList
module LO = Line_oriented
module Log = Dolog.Log

type filename = string

type mode = Regression
          | Classification

let int_of_mode = function
  | Classification -> 1
  | Regression -> 3

let train
    ?debug:(debug = false)
    ?nprocs:(nprocs = 1)
    (mode: mode)
    (nb_trees: int)
    (mtry: int option)
    (data_fn: filename)
    (dep_var_name: string)
    (model_out_fn: filename): bool =
  let mtry_str = match mtry with
    | None -> ""
    | Some m -> sprintf "--mtry %d" m in
  let cmd =
    sprintf
      "ml_rf_ranger %s --file %s --depvarname %s --treetype %d --ntree %d \
       %s --write --outprefix %s --nthreads %d"
      (if debug then "--verbose" else "")
      data_fn
      dep_var_name
      (int_of_mode mode)
      nb_trees
      mtry_str
      model_out_fn
      nprocs in
  Log.info "cmd: %s" cmd;
  let status, log = BatUnix.run_and_read cmd in
  Log.info "%s" log;
  match status with
  | WEXITED 0 ->
    (Sys.rename (model_out_fn ^ ".forest") model_out_fn;
     true)
  | _ -> false

let robust_float_of_string s =
  Scanf.sscanf (BatString.strip s) "%f" (fun x -> x)

let alpha_start =
  Re.Str.regexp "^[a-zA-Z]"

let ok_line l =
  (l <> "") && (not (Re.Str.string_match alpha_start l 0))

let read_raw_class_predictions nb_trees fn =
  let pred_strings =
    (* keep only numeric lines; they don't start with a letter;
       remove empty lines *)
    LO.filter fn ok_line in
  let nb_preds = L.length pred_strings in
  Log.info "nb integer preds: %d" (L.length pred_strings);
  let pred_classes = L.map robust_float_of_string pred_strings in
  let nb_samples = nb_preds / nb_trees in
  Log.info "nb samples: %d" nb_samples;
  let preds = A.of_list pred_classes in
  let res = ref [] in
  for samp_i = 0 to nb_samples - 1 do
    (* gather class predictions for this sample *)
    let curr_preds = ref [] in
    for tree_j = 0 to nb_trees - 1 do
      let offset = (tree_j * nb_samples) + samp_i in
      curr_preds := preds.(offset) :: !curr_preds
    done;
    (* compute mean and stddev *)
    let avg = L.favg !curr_preds in
    let std = Utls.stddev !curr_preds in
    res := (avg, std) :: !res
  done;
  L.rev !res

let predict
    ?debug:(debug = false)
    ?nprocs:(nprocs = 1)
    (nb_trees: int)
    (data_fn: filename)
    (model_fn: filename): (float * float) list option =
  let predictions_fn = Filename.temp_file "oranger_" "" in
  let cmd =
    sprintf
      "ml_rf_ranger %s \
       --file %s --predict %s --nthreads %d --outprefix %s --predall"
      (if debug then "--verbose" else "")
      data_fn
      model_fn
      nprocs
      predictions_fn in
  Log.info "cmd: %s" cmd;
  let status, log = BatUnix.run_and_read cmd in
  Log.info "%s" log;
  match status with
  | WEXITED 0 ->
    begin
      let raw_preds_fn = predictions_fn ^ ".prediction" in
      Some (read_raw_class_predictions nb_trees raw_preds_fn)
    end
  | _ -> None
