open OUnit

module type TestSuite = sig
    val name: string
    val tests: test list
end

let register_tests (module T : TestSuite) =
    T.name >::: T.tests

let _ =
    "Bson main test suite" >:::
    List.map register_tests
             [ (module Encoding_test); (module Decoding_test) ]
    |> run_test_tt_main
