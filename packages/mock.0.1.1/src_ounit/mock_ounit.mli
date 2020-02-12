(**
   Check that the calls correspond exactly to this list.
   Parameters are similar to [assert_equal].
*)
val assert_calls_are:
  ctxt:OUnit2.test_ctxt ->
  cmp:('args -> 'args -> bool) ->
  printer:('args -> string) ->
  'args list ->
  ('args, 'ret) Mock.t ->
  unit

(**
   Raise an exception unless the mock has been called exactly once
   with these arguments.
   Parameters are similar to [assert_equal].
*)
val assert_called_once_with:
  ctxt:OUnit2.test_ctxt ->
  cmp:('args -> 'args -> bool) ->
  printer:('args -> string) ->
  'args ->
  ('args, 'ret) Mock.t ->
  unit
