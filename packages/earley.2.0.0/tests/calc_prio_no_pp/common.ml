open Earley_core

type kind = Quick | Normal | Full | None
let kind = ref None

let output = ref false

let test_cases (a, b, c) =
  match !kind with
  | Quick  -> a
  | Normal -> b
  | Full   -> c
  | None   -> b

let spec =
  [ ("--debug" , Arg.Set_int Earley.debug_lvl       , "set debug lvl"    )
  ; ("--quick" , Arg.Unit (fun () -> kind := Quick) , "quick tests"      )
  ; ("--normal", Arg.Unit (fun () -> kind := Normal), "normal tests"     )
	; ("--full"  , Arg.Unit (fun () -> kind := Full)  , "full tests (long)")
	; ("--out"   , Arg.Set output                     , "output the test"  ) ]

let _  =
  Arg.parse spec
    (fun _ -> raise (Arg.Bad "extra arguments"))
	  "run unit tests on decap combinators"
