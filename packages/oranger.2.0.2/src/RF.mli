
type filename = string

type mode = Regression
          | Classification

val train: ?debug:bool -> ?nprocs:int ->
  mode -> int -> filename -> string -> filename -> bool
(** [train ~debug ~nprocs mode nb_trees data_fn dep_var_name out_fn]
    will train a Random Forests model with [nb_trees], reading
    training data from [data_fn] and using [mode] (either Regression
    or Classification).
    [dep_var_name] is the name of the column holding the target
    value (you want to predict that value with your model later on)
    in [data_fn].
    [data_fn] is a space-separated CSV file, with first line
    as its header (i.e. the names of all columns).
    If training in parallel ([nprocs] > 1) then [nprocs] threads are used.
    The trained model will be stored in [out_fn].
    The [debug] flag controls the verbosity of the underlying C++
    software (ranger) which is really doing all the work. *)

val predict: ?debug:bool -> ?nprocs:int ->
  int -> filename -> filename -> (float * float) list option
(** [predict ~debug ~nprocs nb_trees data_fn model_fn]
    will optionally return a list of (pred_val, stddev).
    I.e. predicted values along with their standard deviation.
    The [debug] flag controls the verbosity of the underlying C++
    software (ranger).
    If predicting in parallel ([nprocs] > 1), then [nprocs] threads are used.
    [nb_trees] is the number of trees of your trained model.
    [data_fn] is the CSV file holding your test data.
    The column in [data_fn] holding the target value will be ignored.
    [model_fn] is a file where you previously stored a (trained) model. *)
