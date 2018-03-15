
type gamma = float

type kernel = RBF of gamma
            | Linear

type filename = string

type nb_columns = int

type sparsity = Dense
              | Sparse of nb_columns

(** [train ~cost kernel data_fn labels_fn] will train a binary
    SVM classifier with the given RBF or Linear kernel
    with parameter [cost] on the data in [data_fn] with labels
    in [labels_fn].
    [data_fn] is a dense numerical matrix dumped in a tab-separated text file
    without any format header. Rows are observations, columns are features.
    [labels_fn] is a vector of tab-separated "1" or "-1" integer labels
    in a text file, without any format header.
    Column [i] in [labels_fn] is the corresponding label of line [i]
    in [data_fn]. *)
val train: ?debug:bool -> sparsity -> cost:float -> kernel -> filename -> filename -> Result.t

(** [predict train_result to_predict_data_fn] will run the previously trained
    SVM model on the new data stored in [to_predict_data_fn].
    [to_predict_data_fn] must follow the same format than [data_fn]
    used while training.
    On success, a filename is returned. This text file contains the predicted
    decision values, one per line of [to_predict_data_fn]. *)
val predict: ?debug:bool -> sparsity -> Result.t -> filename -> Result.t

(** [read_predictions result] will decode predicted decision values
    in [result], or crash if the previous call to [predict]
    was not successful.
    Upon success and if [not debug], the file containing the
    predicted decision values is removed. *)
val read_predictions: ?debug:bool -> Result.t -> float list
