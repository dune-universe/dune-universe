open Lambda_streams

let signal = Test_Signal.signal

let finite_sync = Test_Finite_Sync.finite_sync

let test_pure _ =
  Alcotest.(check (finite_sync int))
    ""
    (Sync.pure 123 |> Finite.Sync.take' 3)
    (Finite.Sync.from_list [123; 123; 123])

let test_enumerate _ =
  Alcotest.(check (finite_sync int))
    ""
    (Sync.enumerate () |> Finite.Sync.take' 3)
    (Finite.Sync.from_list [1; 2; 3])

let test_next _ =
  let stream = Sync.enumerate () in
  Alcotest.(check int) "" (Sync.next stream) 1;
  Alcotest.(check int) "" (Sync.next stream) 2;
  Alcotest.(check int) "" (Sync.next stream) 3

let test_accumulate _ =
  Alcotest.(check int) "" (Sync.pure 123 |> Sync.accumulate 1 ( + ) 0) 123;
  Alcotest.(check int) "" (Sync.pure 123 |> Sync.accumulate 2 ( + ) 0) 246

let test_map _ =
  Alcotest.(check (finite_sync int))
    ""
    (Sync.enumerate () |> Sync.map (( * ) 2) |> Finite.Sync.take' 5)
    (Finite.Sync.from_list [2; 4; 6; 8; 10])

let test_scan _ =
  Alcotest.(check (finite_sync int))
    ""
    (Sync.enumerate () |> Sync.scan ( + ) 0 |> Finite.Sync.take' 5)
    (Finite.Sync.from_list [1; 3; 6; 10; 15])

let test_mutator _ =
  let input, output = Sync.make_mutator ~initial:"foo" in
  Alcotest.(check string) "" (Sync.next input) "foo";
  output |> Sync.send "bar";
  Alcotest.(check string) "" (Sync.next input) "bar";
  output |> Sync.send "baz";
  Alcotest.(check string) "" (Sync.next input) "baz"

let suite =
  [
    "make_mutator", `Quick, test_mutator;
    "pure", `Quick, test_pure;
    "enumerate", `Quick, test_enumerate;
    "next", `Quick, test_next;
    "accumulate", `Quick, test_accumulate;
    "map", `Quick, test_map;
    "scan", `Quick, test_scan;
  ]
