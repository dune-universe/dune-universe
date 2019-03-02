open! Import
open! Topological_sort
module Node = Core_kernel.Int

let test ?(should_print = true) ?(nodes = []) edges =
  let edges = List.map edges ~f:(fun (from, to_) -> { Edge.from; to_ }) in
  let result = sort (module Node) nodes edges in
  if should_print then print_s [%sexp (result : Node.t list Or_error.t)]
;;

let%expect_test "nodes, but no edges" =
  test ~nodes:[ 1 ] [];
  [%expect {|
    (Ok (1)) |}]
;;

let%expect_test "basic graphs" =
  test [];
  [%expect {|
    (Ok ()) |}];
  test [ 1, 2 ];
  [%expect {|
    (Ok (1 2)) |}];
  test [ 1, 2; 2, 3 ];
  [%expect {|
    (Ok (1 2 3)) |}];
  test [ 2, 3; 1, 2 ];
  [%expect {|
    (Ok (1 2 3)) |}];
  test [ 1, 2; 1, 3 ];
  [%expect {|
    (Ok (1 2 3)) |}];
  test [ 1, 2; 1, 3; 2, 3 ];
  [%expect {|
    (Ok (1 2 3)) |}];
  test [ 1, 2; 2, 3; 1, 3 ];
  [%expect {|
    (Ok (1 2 3)) |}];
  test [ 1, 1 ];
  [%expect {|
    (Error ("Topological_sort.sort encountered cycle" (1))) |}];
  test [ 1, 2; 2, 1 ];
  [%expect {|
    (Error ("Topological_sort.sort encountered cycle" (2 1))) |}];
  test [ 1, 2; 2, 3; 3, 1 ];
  [%expect {|
    (Error ("Topological_sort.sort encountered cycle" (3 1 2))) |}]
;;

let%expect_test "after respecting the order, follows [Node.compare] with isolated nodes \
                 at the end"
  =
  let nodes = [ 4; 3; 2; 1 ] in
  let test = test ~nodes in
  test [];
  [%expect {|
    (Ok (1 2 3 4)) |}];
  test [ 3, 4 ];
  [%expect {|
    (Ok (3 4 1 2)) |}];
  test [ 2, 4 ];
  [%expect {|
    (Ok (2 4 1 3)) |}];
  test [ 1, 4 ];
  [%expect {|
    (Ok (1 4 2 3)) |}];
  test [ 2, 3 ];
  [%expect {|
    (Ok (2 3 1 4)) |}];
  test [ 3, 4; 1, 2 ];
  [%expect {|
    (Ok (1 2 3 4)) |}]
;;

let%expect_test "all graphs with 5 or fewer nodes" =
  let num_tests_run = ref 0 in
  for num_nodes = 0 to 5 do
    let rec loop from edges =
      if from = 0
      then (
        test edges ~should_print:false;
        incr num_tests_run)
      else
        for to_ = 1 to num_nodes do
          loop (from - 1) ((from, to_) :: edges);
          loop (from - 1) edges
        done
    in
    loop num_nodes []
  done;
  print_s [%message (num_tests_run : int ref)];
  [%expect {|
    (num_tests_run 104331) |}]
;;
