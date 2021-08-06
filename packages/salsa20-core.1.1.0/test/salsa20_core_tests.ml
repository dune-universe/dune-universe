let test_salsa20_core ~f ~input ~output =
  let open Cstruct in
  let input = of_hex input
  and output = output |> of_hex |> to_string in
  (fun () ->
     let output2 = input |> f |> to_string in
     Alcotest.check Alcotest.string "Salsa20 Core test" output output2)

(* Salsa20/8 Core *)
let test_salsa20_8_core ~input ~output =
  test_salsa20_core ~f:Salsa20_core.salsa20_8_core ~input ~output

let salsa20_8_core_test1 =
  test_salsa20_8_core
    ~input:"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    ~output:"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

let salsa20_8_core_test2 =
  test_salsa20_8_core
    ~input:"7e879a214f3ec9867ca940e641718f26baee555b8c61c1b50df846116dcd3b1dee24f319df9b3d8514121e4b5ac5aa3276021d2909c74829edebc68db8b8c25e"
    ~output:"a41f859c6608cc993b81cacb020cef05044b2181a2fd337dfd7b1c6396682f29b4393168e3c9e6bcfe6bc5b7a06d96bae424cc102c91745c24ad673dc7618f81"

let salsa20_8_core_tests = [
  "Test Case 1", `Quick, salsa20_8_core_test1;
  "Test Case 2", `Quick, salsa20_8_core_test2;
]

(* Salsa20/12 Core *)
let test_salsa20_12_core ~input ~output =
  test_salsa20_core ~f:Salsa20_core.salsa20_12_core ~input ~output

let salsa20_12_core_test1 =
  test_salsa20_12_core
    ~input:"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    ~output:"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

let salsa20_12_core_tests = [
  "Test Case 1", `Quick, salsa20_12_core_test1;
]

(* Salsa20/20 Core *)
let test_salsa20_20_core ~input ~output =
  test_salsa20_core ~f:Salsa20_core.salsa20_20_core ~input ~output

let salsa20_20_core_test1 =
  test_salsa20_20_core
    ~input:"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    ~output:"00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"

let salsa20_20_core_test2 =
  test_salsa20_20_core
    ~input:"d39f0d734c3752b70375de25bfbbea8831edb330016ab2dbafc7a6305610b3cf1ff0203f0f535da174933071ee37cc244fc9eb4f03519c2fcb1af4f358766836"
    ~output:"6d2ab2a89cf0f8eea8c4becb1a6eaa9a1d1d961a961eebf9bea3fb30459033397628989db4391b5e6b2aec231b6f7272dbece8876f9b6e1218e85f9eb31330ca"

let salsa20_20_core_test3 =
  test_salsa20_20_core
    ~input:"587668364fc9eb4f03519c2fcb1af4f3bfbbea88d39f0d734c3752b70375de255610b3cf31edb330016ab2dbafc7a630ee37cc241ff0203f0f535da174933071"
    ~output:"b31330cadbece8876f9b6e1218e85f9e1a6eaa9a6d2ab2a89cf0f8eea8c4becb459033391d1d961a961eebf9bea3fb301b6f72727628989db4391b5e6b2aec23"

let salsa20_20_core_tests = [
  "Test Case 1", `Quick, salsa20_20_core_test1;
  "Test Case 2", `Quick, salsa20_20_core_test2;
  "Test Case 3", `Quick, salsa20_20_core_test3;
]

(* Salsa20/20 Core with 1M iterations *)
let test_salsa20_20_core_1M ~input ~output =
  let open Cstruct in
  let input = of_hex input
  and output = output |> of_hex |> to_string in
  (fun () ->
     let o = ref input in
     for _ = 1 to 1_000_000 do
       o := Salsa20_core.salsa20_20_core !o
     done;
     let output2 = !o |> to_string in
     Alcotest.check Alcotest.string "Salsa20 Core test" output output2)

let salsa20_20_core_1M_test1 =
  test_salsa20_20_core_1M
    ~input:"067c539226bf093204a12fde7ab6dfb94b1b00d8107a0759a2686593d515365fe1fd8bb0698417744c29b0cfdd229d6c5e5e63345a755bdc92beef8fc4b082ba"
    ~output:"081226c7774cd743ad7f90a267d4b0d9c013e9219fc59aa080f3db41ab8887e17b0b4456ed52149b85bd0953a774c24e7a7fc3b9b9ccbc5af509b7f8e255f568"

let salsa20_20_core_1M_tests = [
  "Test Case 1", `Slow, salsa20_20_core_1M_test1;
]


let () =
  Alcotest.run "Salsa20 Core Tests" [
    "Salsa20 8 Core tests", salsa20_8_core_tests;
    "Salsa20 12 Core tests", salsa20_12_core_tests;
    "Salsa20 20 Core tests", salsa20_20_core_tests;
    "Salsa20 20 Core with 1M iteration tests", salsa20_20_core_1M_tests;
  ]
