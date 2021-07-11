open Base
open Non_empty_list

let non_empty_list testable =
  Alcotest.testable
    (Non_empty_list.pp (Alcotest.pp testable))
    (Non_empty_list.equal (Alcotest.equal testable))


module type Test = sig
  val test : unit Alcotest.test
end

module Init : Test = struct
  let test_1 () =
    let expected = Invalid_argument "Non_empty_list.init 0" in
    let test () = ignore (init 0 ~f:Fn.id) in
    Alcotest.check_raises "raises Invalid_argument" expected test


  let test_2 () =
    let expected = List.range 0 10 in
    let order = ref [] in
    ignore
      (init 10 ~f:(fun i ->
           order := i :: !order;
           i));
    Alcotest.(check @@ list int) "same list" expected !order


  let test_3 () =
    let expected = [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
    init 8 ~f:(fun i -> i + 1) |> Alcotest.(check @@ non_empty_list int) "same list" expected


  let test : unit Alcotest.test =
    Alcotest.(
      ( "init"
      , [ test_case "raises Invalid_argument if n = 0" `Quick test_1
        ; test_case "right to left evaluation order" `Quick test_2
        ; test_case "correctness 1" `Quick test_3
        ] ))
end

module Of_list : Test = struct
  let test_1 () =
    let expected = None in
    of_list [] |> Alcotest.(check @@ option @@ non_empty_list int) "none" expected


  let test_2 () =
    let expected = Some [ 1; 2; 3; 4 ] in
    of_list [ 1; 2; 3; 4 ] |> Alcotest.(check @@ option @@ non_empty_list int) "same list" expected


  let test : unit Alcotest.test =
    Alcotest.(
      "of_list", [ test_case "empty list" `Quick test_1; test_case "non empty list" `Quick test_2 ])
end

module Length : Test = struct
  (* tests the law length x :: xs = length xs + 1 *)
  let test_1 () =
    let expected = 1 + length [ 2; 3; 4; 5 ] in
    length [ 1; 2; 3; 4; 5 ] |> Alcotest.(check int) "same length" expected


  let test : unit Alcotest.test =
    Alcotest.("length", [ test_case "law: length (x :: xs) = length xs + 1" `Quick test_1 ])
end

module Is_singleton : Test = struct
  let test_1 () = is_singleton [ 2; 3 ] |> Alcotest.(check bool) "same bool" false
  let test_2 () = is_singleton [ 1 ] |> Alcotest.(check bool) "same bool" true

  let test : unit Alcotest.test =
    Alcotest.(
      ( "is_singleton"
      , [ test_case "non_singleton" `Quick test_1; test_case "singleton" `Quick test_2 ] ))
end

module Cons : Test = struct
  let test_1 () =
    let expected = [ 2; 1 ] in
    cons 2 [ 1 ] |> Alcotest.(check @@ non_empty_list int) "same list" expected


  let test : unit Alcotest.test = Alcotest.("cons", [ test_case "correctness" `Quick test_1 ])
end

module Append : Test = struct
  let test_1 () =
    append (append [ 1; 2 ] [ 3 ]) [ 4; 5 ]
    |> Alcotest.(check @@ non_empty_list int) "same list" (append [ 1; 2 ] (append [ 3 ] [ 4; 5 ]))


  let test_2 () =
    let expected = [ 1; 2; 3; 4; 5 ] in
    append [ 1; 2 ] [ 3; 4; 5 ] |> Alcotest.(check @@ non_empty_list int) "same list" expected


  let test : unit Alcotest.test =
    Alcotest.(
      "append", [ test_case "assocaitvity" `Quick test_1; test_case "correctness" `Quick test_2 ])
end

module Hd : Test = struct
  let test_1 () =
    let expected = 1 in
    hd [ 1; 2; 3; 4; 5 ] |> Alcotest.(check int) "same int" expected


  let test : unit Alcotest.test = Alcotest.("hd", [ test_case "correctness" `Quick test_1 ])
end

module Tl : Test = struct
  let test_1 () =
    let expected : int list = [ 2; 3; 4; 5 ] in
    tl [ 1; 2; 3; 4; 5 ] |> Alcotest.(check @@ list int) "same list" expected


  let test : unit Alcotest.test = Alcotest.("tl", [ test_case "correctness" `Quick test_1 ])
end

module Last : Test = struct
  let test_1 () =
    let expected = 1 in
    last [ 1 ] |> Alcotest.(check int) "same int" expected


  let test_2 () =
    let expected = 2 in
    last [ 1; 2 ] |> Alcotest.(check int) "same int" expected


  let test_3 () =
    let expected = 10 in
    last [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] |> Alcotest.(check int) "same int" expected


  let test : unit Alcotest.test =
    Alcotest.(
      ( "last"
      , [ test_case "1 element" `Quick test_1
        ; test_case "2 element" `Quick test_2
        ; test_case "many element" `Quick test_3
        ] ))
end

module Nth : Test = struct
  let t = [ 1; 2; 3; 4 ]

  let test_1 () =
    let expected = Some (hd t) in
    nth t 0 |> Alcotest.(check @@ option int) "same option" expected


  let test_2 () =
    let expected = None in
    nth t (-1) |> Alcotest.(check @@ option int) "same option" expected


  let test_3 () =
    let expected = None in
    nth t 10 |> Alcotest.(check @@ option int) "same option" expected


  let test_4 () =
    let expected = Some 2 in
    nth t 1 |> Alcotest.(check @@ option int) "same option" expected


  let test_5 () =
    let expected = Some 3 in
    nth t 2 |> Alcotest.(check @@ option int) "same option" expected


  let test_6 () =
    let expected = Some 4 in
    nth t 3 |> Alcotest.(check @@ option int) "same option" expected


  let test : unit Alcotest.test =
    Alcotest.(
      ( "nth"
      , [ test_case "nth 0 = hd" `Quick test_1
        ; test_case "returns None if n < 0" `Quick test_2
        ; test_case "returns None if n > length" `Quick test_3
        ; test_case "correctness 1" `Quick test_4
        ; test_case "correctness 2" `Quick test_5
        ; test_case "correctness 3" `Quick test_6
        ] ))
end

module Rev : Test = struct
  let t = [ 1; 2; 3; 4 ]

  let test_1 () =
    let expected = t in
    rev (rev t) |> Alcotest.(check @@ non_empty_list int) "same list" expected


  let test_2 () =
    let expected = t |> to_list |> List.rev in
    rev t |> to_list |> Alcotest.(check @@ list int) "same list" expected


  let test : unit Alcotest.test =
    Alcotest.(
      ( "rev"
      , [ test_case "reversing twice is identity" `Quick test_1
        ; test_case "identical semantics to List.rev" `Quick test_2
        ] ))
end

let () =
  Alcotest.run
    "non_empty_list"
    [ Init.test
    ; Of_list.test
    ; Length.test
    ; Is_singleton.test
    ; Cons.test
    ; Append.test
    ; Hd.test
    ; Tl.test
    ; Last.test
    ; Nth.test
    ; Rev.test
    ]
