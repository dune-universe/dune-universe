open Base
open Lacaml.D
open Caml.Format
open Lacaml.Io
open Libsvm

(* This program is an OCaml translation of the Python script svm_test.py which
   is included in an earlier version of the LIBSVM package, e.g. libsvm-2.88. *)
let () =
  let targets = Vec.of_array [|0.; 1.; 1.; 2.|] in  (* a three-class problem *)
  let samples = Mat.of_array [|
    [|0.;0.|]; [|0.; 1.|]; [|1.;0.|]; [|1.; 1.|]
  |] in
  let inputs = Mat.transpose_copy samples in
  let problem = Svm.Problem.create ~x:samples ~y:targets in
  let n_samples = Svm.Problem.get_n_samples problem in
  let kernels = [ `LINEAR; `POLY; `RBF ] in
  let kernel_name = function
    | `LINEAR -> "linear"
    | `POLY   -> "polynomial"
    | `RBF    -> "rbf"
    | _       -> assert false
  in
  let print_error_rate kernel =
    let model = Svm.train ~kernel ~c:10. ~weights:[(1,10.);(0,1.)] problem in
    let preds = Svm.predict model ~x:samples in
    let errors = n_samples - Stats.calc_n_correct targets preds in
    printf "##########################################\n";
    printf " kernel %s: error rate = %d / %d\n" (kernel_name kernel) errors n_samples;
    printf "##########################################\n";
  in
  List.iter kernels ~f:print_error_rate;
  let model = Svm.train ~kernel:`RBF ~c:10. problem in
  let input = Mat.col inputs 1 in
  printf "@[##########################################@\n";
  printf " Decision values of predicting: @[%a@]@\n" pp_rfvec input;
  printf "##########################################@]@\n";
  printf "Number of Classes: %d\n" (Svm.Model.get_n_classes model);
  let dec_mat = Svm.predict_values model ~x:input in
  let labels = Svm.Model.get_labels model in
  List.iter (List.cartesian_product labels labels) ~f:(fun (i, j) ->
    if j > i then printf "{%d, %d} = %f\n" i j dec_mat.(i).(j));
  let model = Svm.train ~kernel:`RBF ~c:10. ~probability:true problem in
  let input = Mat.col inputs 2 in
  let pred_label, prob_estimates = Svm.predict_probability model ~x:input in
  printf "@[##########################################@\n";
  printf " Probability estimates of predicting: @[%a@]@\n" pp_rfvec input;
  printf "##########################################@]@\n";
  printf "predicted class: %d\n" (Int.of_float pred_label);
  List.iter labels ~f:(fun i ->
    printf "prob(label=%d) = %g\n" i prob_estimates.(i));
  printf "\n##########################################\n";
  printf " Precomputed kernels\n";
  printf "##########################################\n";
  let k = Mat.of_array [|
    [|1.; 0.; 0.; 0.; 0.|]; [|2.; 0.; 1.; 0.; 1.|];
    [|3.; 0.; 0.; 1.; 1.|]; [|4.; 0.; 1.; 1.; 2.|];
  |] in
  let problem = Svm.Problem.create_k ~k ~y:targets in
  let model = Svm.train ~kernel:`PRECOMPUTED ~c:10.
    ~weights:[(1,10.);(0,1.)] problem
  in
  let pred_labels = Svm.predict model ~x:k in
  printf "predicted classes: @[%a@]@\n" pp_rfvec pred_labels;
