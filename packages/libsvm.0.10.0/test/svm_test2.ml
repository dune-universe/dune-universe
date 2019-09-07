open Core_kernel

let f () =
  let open Lacaml.D in
  let d = 5 in
  let n = 10 in
  let vec fst =
    let f _ =
      if Random.float 1. < 0.4 then 0.
      else Random.float 1.
    in
    Array.(append [|fst|] (init (d - 1) ~f))
  in
  let x_pos = Array.init n ~f:(fun _ -> vec 1.) in
  let x_neg = Array.init n ~f:(fun _ -> vec (-1.)) in
  let x = Mat.of_array (Array.append x_pos x_neg) in
  let y = Vec.init (2 * n) (fun i -> if i < n then 1. else 0.) in
  let _prob = Libsvm.Svm.Problem.create_dense ~x ~y in
  Gc.full_major () ;
  let prob = Libsvm.Svm.Problem.create_dense ~x ~y in
  Libsvm.Svm.Problem.print prob ;
  let res =
    Libsvm.Svm.cross_validation ~svm_type:`C_SVC ~kernel:`RBF prob ~n_folds:5
    |> Libsvm.Stats.calc_accuracy y
  in
  printf "CV Accuracy: %f\n" res

let () =
  let open Lacaml.D in
  let d = 5 in
  let n = 10 in
  let vec fst =
    let f i =
      if Random.float 1. < 0.4 then None
      else Some (i + 1, Random.float 1.)
    in
    (0, fst) :: List.(init (d - 1) ~f |> filter_opt)
  in
  let x_pos = Array.init n ~f:(fun _ -> vec 1.) in
  let x_neg = Array.init n ~f:(fun _ -> vec (-1.)) in
  let x = Array.append x_pos x_neg in
  let y = Vec.init (2 * n) (fun i -> if i < n then 1. else 0.) in
  let _prob = Libsvm.Svm.Problem.create ~x ~y in
  Gc.full_major () ;
  let prob = Libsvm.Svm.Problem.create ~x ~y in
  Libsvm.Svm.Problem.print prob ;
  Libsvm.Svm.Problem.scale prob ;
  Libsvm.Svm.Problem.print prob ;
  let res =
    Libsvm.Svm.cross_validation ~svm_type:`C_SVC ~kernel:`RBF prob ~n_folds:5
    |> Libsvm.Stats.calc_accuracy y
  in
  printf "CV Accuracy: %f\n" res
