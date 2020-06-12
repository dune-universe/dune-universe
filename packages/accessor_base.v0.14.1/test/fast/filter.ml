open! Core_kernel
open! Import

let%expect_test _ =
  Accessor.mapi
    (Accessor.List.eachi
     @> Accessor.filter_index (fun [ i ] -> i % 2 = 0)
     @> Accessor.Tuple2.snd)
    [ "a", 1; "b", 2; "c", 3; "d", 4; "e", 5 ]
    ~f:(fun [ (_ : int) ] n -> n * 10)
  |> [%sexp_of: (string * int) list]
  |> print_s;
  [%expect {| ((a 10) (b 2) (c 30) (d 4) (e 50)) |}]
;;
