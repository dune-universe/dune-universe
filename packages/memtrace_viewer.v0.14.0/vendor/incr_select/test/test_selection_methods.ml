open Core_kernel
open Import

module S = Incr.Select

let setup_test ~f ~list_size ~init run =
  let var = Incr.Var.create init in
  let value = Incr.map ~f (Incr.Var.watch var) in
  let gen_incr = Staged.unstage (run value) in
  let gen_incr key =
    let incr = gen_incr key in
    (* Make sure we are getting all of the changes from the underlying
       incremental *)
    Incr.set_cutoff incr Incr.Cutoff.never;
    incr
  in
  let stabilized_set = ref Int.Set.empty in
  let incrs =
    List.init list_size ~f:(fun i ->
      let%map value = gen_incr i in
      stabilized_set := Set.add !stabilized_set i;
      value
    )
  in
  var, stabilized_set, Incr.observe (Incr.all incrs)

let print_boolean_list l =
  List.iteri l ~f:(fun idx value ->
    if value then printf "%d " idx
  );
  printf "\n"

let print_stabilized_set s =
  Set.iter !s ~f:(fun idx ->
    printf "%d " idx
  );
  printf "\n";
  s := Int.Set.empty

let%expect_test "select_one" =
  let var, stabilized, o =
    setup_test
      ~list_size:5
      ~init:2
      ~f:Fn.id
      (S.select_one (module Int))
  in
  let print_list () = print_boolean_list (Incr.Observer.value_exn o) in

  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 1 2 3 4 |}];

  print_list ();
  [%expect {| 2 |}];

  Incr.Var.set var 3;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 2 3 |}];

  print_list ();
  [%expect {| 3 |}];

  Incr.Var.set var 5;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 3 |}];

  print_list ();
  [%expect {| |}];

  Incr.Var.set var 0;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 |}];

  print_list ();
  [%expect {| 0 |}]

let%expect_test "select_one'" =
  let var, stabilized, o =
    setup_test
      ~list_size:5
      ~init:None
      ~f:Fn.id
      (S.select_one' (module Int))
  in
  let print_list () = print_boolean_list (Incr.Observer.value_exn o) in

  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 1 2 3 4 |}];

  print_list ();
  [%expect {| |}];

  Incr.Var.set var (Some 3);
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 3 |}];

  print_list ();
  [%expect {| 3 |}];

  Incr.Var.set var (Some 2);
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 2 3 |}];

  print_list ();
  [%expect {| 2 |}];

  Incr.Var.set var None;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 2 |}];

  print_list ();
  [%expect {| |}]

let%expect_test "select_one_value" =
  let var, stabilized, data =
    setup_test
      ~f:(fun i -> i, i*i)
      ~list_size:5
      ~init:0
      (S.select_one_value ~default:(-1) (module Int))
  in
  let print_list () =
    List.iter (Incr.Observer.value_exn data) ~f:(fun value ->
      printf "%d " value
    );
    printf "\n"
  in

  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 1 2 3 4 |}];

  print_list ();
  [%expect {| 0 -1 -1 -1 -1 |}];

  Incr.Var.set var 3;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 3 |}];

  print_list ();
  [%expect {| -1 -1 -1 9 -1 |}]

let%expect_test "select_one_value'" =
  let var, stabilized, data =
    setup_test
      ~f:(Option.map ~f:(fun i -> (i, i*i)))
      ~list_size:5
      ~init:None
      (S.select_one_value' ~default:(-1) (module Int))
  in
  let print_list () =
    List.iter (Incr.Observer.value_exn data) ~f:(fun value ->
      printf "%d " value
    );
    printf "\n"
  in

  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 1 2 3 4 |}];

  print_list ();
  [%expect {| -1 -1 -1 -1 -1 |}];

  Incr.Var.set var (Some 3);
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 3 |}];

  print_list ();
  [%expect {| -1 -1 -1 9 -1 |}]

let%expect_test "select_many" =
  let var, stabilized, data =
    setup_test
      ~f:(fun i -> List.init i ~f:(fun x -> i+x)) (* Indices i..2*i-1 *)
      ~list_size:10
      ~init:3
      (S.select_many (module Int))
  in
  let print_list () = print_boolean_list (Incr.Observer.value_exn data) in

  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 1 2 3 4 5 6 7 8 9 |}];

  print_list ();
  [%expect {| 3 4 5 |}];

  Incr.Var.set var 2;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 2 4 5 |}];

  print_list ();
  [%expect {| 2 3 |}];

  Incr.Var.set var 7;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 2 3 7 8 9 |}]; (* cutoff because only 10 watchers *)

  print_list ();
  [%expect {| 7 8 9 |}]

let%expect_test "select_many_values" =
  let var, stabilized, data =
    setup_test
      ~f:(fun i -> [i, i+2; i+2, i*i])
      ~list_size:10
      ~init:3
      (S.select_many_values ~default:0 (module Int))
  in
  let print_list () =
    List.iter (Incr.Observer.value_exn data) ~f:(fun data ->
      printf "%d " data
    );
    printf "\n"
  in

  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 0 1 2 3 4 5 6 7 8 9 |}];

  print_list ();
  [%expect {| 0 0 0 5 0 9 0 0 0 0 |}];

  Incr.Var.set var 5;
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 3 5 7 |}];

  print_list ();
  [%expect {| 0 0 0 0 0 7 0 25 0 0 |}];

  Incr.Var.set var (-1);
  Incr.stabilize ();
  print_stabilized_set stabilized;
  [%expect {| 1 5 7 |}];

  print_list ();
  [%expect {| 0 1 0 0 0 0 0 0 0 0 |}]
