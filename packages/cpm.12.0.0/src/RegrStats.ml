(* Performance measures for regression models
   cf. chapter 12 "regression models" in book
   Varnek, A. ed., 2017. Tutorials in chemoinformatics. John Wiley & Sons. *)

module A = BatArray
module L = BatList

let square x =
  x *. x

(** Root Mean Squared Error
    [rmse exp pred] *)
let rmse (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_squared_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. square (x -. y)
      ) 0.0 a1 in
  sqrt (sum_squared_diffs /. (float n))

(** Mean Absolute Error
    [mae exp pred] *)
let mae (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_abs_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. abs_float (x -. y)
      ) 0.0 a1 in
  sum_abs_diffs /. (float n)

(** standard deviation of residuals
    [std_dev_res exp pred] *)
let std_dev_res (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_squared_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. square (x -. y)
      ) 0.0 a1 in
  sqrt (sum_squared_diffs /. (float (n - 2)))

(** coefficient of determination
    [r2 exp pred] *)
let r2 (l1: float list) (l2: float list): float =
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let m = A.length a1 in
  let n = A.length a2 in
  assert(m = n);
  let sum_squared_diffs =
    A.fold_lefti (fun acc i x ->
        let y = a2.(i) in
        acc +. square (x -. y)
      ) 0.0 a1 in
  let sum_squared_exp_diffs =
    let avg_exp = A.favg a1 in
    A.fold_left (fun acc x ->
        acc +. square (x -. avg_exp)
      ) 0.0 a1 in
  1.0 -. (sum_squared_diffs /. sum_squared_exp_diffs)

(** raw Regression Error Characteristic Curve
    (raw means not scaled by a null model)
    [raw_REC_curve exp pred]
    Cf. Bi, J. and Bennett, K.P., 2003.
    Regression error characteristic curves.
    In Proceedings of the 20th international conference on machine learning
    (ICML-03) (pp. 43-50). *)
let raw_REC_curve (l1: float list) (l2: float list): (float * float) list =
  let array_filter_count p a =
    float
      (A.fold_left (fun acc x ->
           if p x then acc + 1 else acc
         ) 0 a) in
  let a1 = A.of_list l1 in
  let a2 = A.of_list l2 in
  let n = A.length a1 in
  let errors =
    A.map2 (fun x y ->
        abs_float (x -. y)
      ) a1 a2 in
  A.sort BatFloat.compare errors;
  let max_err = errors.(n - 1) in
  (* 100 steps on the X axis *)
  let xs = L.frange 0.0 `To max_err 100 in
  (* WARNING: not very efficient algorithm *)
  let m = float n in
  L.map (fun err_tol ->
      let percent_ok =
        let ok_count = array_filter_count (fun err -> err <= err_tol) errors in
        (ok_count /. m) in
      (err_tol, percent_ok)
    ) xs
