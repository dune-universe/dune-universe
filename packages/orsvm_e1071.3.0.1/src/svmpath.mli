
type filename = string

(** [train data_fn labels_fn] will train a binary
    SVM classifier with the linear (dot product) kernel
    and at the same time find out the lambda values to test
    later on in order to find the best one (lambda = 1/C).
    [data_fn] is a dense numerical matrix dumped in a tab-separated text file
    without any format header. Rows are observations, columns are features.
    [labels_fn] is a vector of tab-separated "1" or "-1" integer labels
    in a text file, without any format header.
    Column [i] in [labels_fn] is the corresponding label of line [i]
    in [data_fn]. *)
val train: ?debug:bool -> filename -> filename -> Result.t

(** [read_lambdas train_result] will extract all lambda values that
    need to be tested (on a test set) in order to find the best one.
    If this list is empty then training was not successful. *)
val read_lambdas: ?debug:bool -> Result.t -> float list

(** [predict ~lambda:1.0 train_result to_predict_data_fn]
    will run the previously trained
    SVM model on the new data stored in [to_predict_data_fn].
    [to_predict_data_fn] must follow the same format than [data_fn]
    used while training.
    On success, a filename is returned. This text file contains the predicted
    decision values, one per line of [to_predict_data_fn]. *)
val predict: ?debug:bool -> lambda:float -> Result.t -> filename -> Result.t

(** [read_predictions result] will decode predicted decision values
    in [result], or crash if the previous call to [predict]
    was not successful.
    Upon success and if [not debug], the file containing the
    predicted decision values is removed. *)
val read_predictions: ?debug:bool -> Result.t -> float list
