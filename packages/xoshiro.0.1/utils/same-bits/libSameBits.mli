(** {1 SameBits}

    Utilities to run tests on functions supposed to yield the same answers in
   the same order; practical to test two different implementations of the same
   random number generator. *)

type test_case

val make_test_case :
  name:string ->
  pp:(Format.formatter -> 'a -> unit) ->
  string ->
  (unit -> 'a) ->
  string ->
  (unit -> 'a) ->
  test_case

val run_test_cases : test_case list -> unit

val run_test_case : test_case -> unit
(** Same as {!run_test_cases} for only one case. *)

val print_header : unit -> unit

val run_on_basic :
  string -> (module MakeRandom.Sig.Basic) ->
  string -> (module MakeRandom.Sig.Basic) ->
  unit
(** [run_on_basic r_name (module R) s_name (module S)] compares the modules [R]
   and [S] (with names [r_name] and [s_name] respectively). *)

val run_on_full :
  string -> (module MakeRandom.Sig.Full) ->
  string -> (module MakeRandom.Sig.Full) ->
  unit
(** Same as {!run_on_basic} for modules having the full [Random] interface.
   Since there are possible state manipulations, there are many more scenarios
   to run than with {!run_on_basic}. *)
