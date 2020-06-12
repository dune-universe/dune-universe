open! Core_kernel
open! Import

let%expect_test "many_to_list_field" =
  let sort_some_elems list ~when_index =
    list
    |> Accessor.map
         (Accessor.many_to_list_field
            (Accessor.List.eachi
             @> Accessor.filter_map_index (fun [ i ] ->
               if when_index i then Some [] else None)))
         ~f:(List.sort ~compare:[%compare: int])
    |> [%sexp_of: int list]
    |> print_s
  in
  let xs = [ 1; 9; 7; 2; 5; 8; 6; 0; 3; 4 ] in
  sort_some_elems xs ~when_index:(fun i -> i mod 2 = 0);
  [%expect {| (1 9 3 2 5 8 6 0 7 4) |}];
  sort_some_elems xs ~when_index:(fun i -> i mod 2 = 1);
  [%expect {| (1 0 7 2 5 4 6 8 3 9) |}];
  sort_some_elems xs ~when_index:(fun i -> i < 5);
  [%expect {| (1 2 5 7 9 8 6 0 3 4) |}];
  sort_some_elems xs ~when_index:(fun i -> i >= 5);
  [%expect {| (1 9 7 2 5 0 3 4 6 8) |}];
  sort_some_elems xs ~when_index:(const true);
  [%expect {| (0 1 2 3 4 5 6 7 8 9) |}]
;;
