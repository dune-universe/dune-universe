open OUnit

module type TestSuite = sig
    val name: string
    val tests: test list
end

let register_tests (module T : TestSuite) =
    T.name >::: T.tests

let _ =
    "Ocamlapi main test suite" >:::
    List.map register_tests
             [ (module Router_trie_test)
             ; (module Router_trie_constructor_test) ]
    |> run_test_tt_main
