open OUnit2

let () = run_test_tt_main (
"depqbf">:::[
    "test_depqbf1">::(Test_depqbf1.test_depqbf1);
    "test_depqbf2">::(Test_depqbf2.test_depqbf2);
    "test_depqbf3">::(Test_depqbf3.test_depqbf3);
])