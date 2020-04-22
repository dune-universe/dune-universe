open OUnit2;;

let dummy () octx = assert_equal true true;;

let suite = "lambda" >::: [
	"dummy"	>:: dummy ();	
];;

let () = run_test_tt_main suite;;