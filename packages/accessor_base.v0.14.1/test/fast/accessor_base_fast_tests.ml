open! Core_kernel
open! Import

let test_map accessor at = Accessor.map accessor at ~f:(fun a -> `f a)

let%expect_test "map id" =
  let result = test_map Accessor.id `at in
  print_s [%sexp (result : [ `f of [ `at ] ])];
  [%expect {| (f at) |}]
;;

let%expect_test "map isomorphism" =
  let result =
    test_map
      (Accessor.isomorphism ~get:(fun at -> `get at) ~construct:(fun b -> `construct b))
      `at
  in
  print_s [%sexp (result : [ `construct of [ `f of [ `get of [ `at ] ] ] ])];
  [%expect {| (construct (f (get at))) |}]
;;

let%expect_test "map field" =
  let result =
    test_map
      (Accessor.field ~get:(fun at -> `get at) ~set:(fun at b -> `set (at, b)))
      `at
  in
  print_s [%sexp (result : [ `set of [ `at ] * [ `f of [ `get of [ `at ] ] ] ])];
  [%expect {| (set (at (f (get at)))) |}]
;;

let%expect_test "map variant" =
  let accessor =
    Accessor.variant
      ~match_:(function
        | `at1 -> First `at1
        | `at2 -> Second `at2)
      ~construct:(fun b -> `construct b)
  in
  let run at =
    let result = test_map accessor at in
    print_s [%sexp (result : [ `construct of [ `f of [ `at1 ] ] | `at2 ])]
  in
  run `at1;
  [%expect {| (construct (f at1)) |}];
  run `at2;
  [%expect {| at2 |}]
;;

let%expect_test "map optional" =
  let accessor =
    Accessor.optional
      ~match_:(function
        | `at1 -> First `at1
        | `at2 -> Second `at2)
      ~set:(fun at b -> `set (at, b))
  in
  let run at =
    let result = test_map accessor at in
    print_s [%sexp (result : [ `at2 | `set of [ `at1 | `at2 ] * [ `f of [ `at1 ] ] ])]
  in
  run `at1;
  [%expect {| (set (at1 (f at1))) |}];
  run `at2;
  [%expect {| at2 |}]
;;

let%expect_test "map nonempty" =
  let accessor =
    Accessor.nonempty (fun at ->
      let open Accessor.Nonempty.Let_syntax in
      match at with
      | `at1 x ->
        let%map_open x = access x in
        `at1 x
      | `at2 (x, y) ->
        let%map_open x = access x
        and y = access y in
        `at2 (x, y))
  in
  let run at =
    let result = test_map accessor at in
    print_s
      [%sexp
        (result
         : [ `at1 of [ `f of [ `x | `y | `z ] ]
           | `at2 of [ `f of [ `x | `y | `z ] ] * [ `f of [ `x | `y | `z ] ]
           ])]
  in
  run (`at1 `x);
  [%expect {| (at1 (f x)) |}];
  run (`at2 (`y, `z));
  [%expect {| (at2 ((f y) (f z))) |}]
;;

let%expect_test "map many" =
  let accessor =
    Accessor.many (fun at ->
      let open Accessor.Many.Let_syntax in
      match at with
      | `at0 -> return `at0
      | `at1 x ->
        let%map_open x = access x in
        `at1 x
      | `at2 (x, y) ->
        let%map_open x = access x
        and y = access y in
        `at2 (x, y))
  in
  let run at =
    let result = test_map accessor at in
    print_s
      [%sexp
        (result
         : [ `at0
           | `at1 of [ `f of [ `x | `y | `z ] ]
           | `at2 of [ `f of [ `x | `y | `z ] ] * [ `f of [ `x | `y | `z ] ]
           ])]
  in
  run `at0;
  [%expect {| at0 |}];
  run (`at1 `x);
  [%expect {| (at1 (f x)) |}];
  run (`at2 (`y, `z));
  [%expect {| (at2 ((f y) (f z))) |}]
;;

let%expect_test "map mapper" =
  let accessor =
    Accessor.mapper (fun at ~f ->
      match at with
      | `at0 -> `at0
      | `at1 x -> `at1 (f x)
      | `at2 (x, y) -> `at2 (f x, f y))
  in
  let run at =
    let result = test_map accessor at in
    print_s
      [%sexp
        (result
         : [ `at0
           | `at1 of [ `f of [ `x | `y | `z ] ]
           | `at2 of [ `f of [ `x | `y | `z ] ] * [ `f of [ `x | `y | `z ] ]
           ])]
  in
  run `at0;
  [%expect {| at0 |}];
  run (`at1 `x);
  [%expect {| (at1 (f x)) |}];
  run (`at2 (`y, `z));
  [%expect {| (at2 ((f y) (f z))) |}]
;;
