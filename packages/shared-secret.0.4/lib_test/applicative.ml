open OUnit2
open Shared_secret

(*
  'Cause OCaml has applicative functor semantics, two distinct instances
  of Message(String) share the same abstract type t, which is Message(String).t,
  for this reason, I need to think a way out of it, where every instance, even
  sharing the same dependency (e.g, String), will fail when used one against other
  (e.g, encoding with the first and decoding with the second on the same message will
  be impossible). That thing means I should fake generative functor semantics.

  Additional note:
  ----------------

  Private types in OCaml are somehow generative, but they expose some details, thus,
  allowing upcasts into the super type. The major difference between private types
  and existential types is that private types allow the use outside the module boundary,
  but disallow the creation of such "private" values outside that boundary (existentials
  disallow both use and creation outside).

*)

(***** fixed, but only for OCaml >= 4.02 *****)

module First = struct
  include Message (String) ( )

  let encoder ( ) = Encoder.encode "Hello, OCaml!"
  let decoder     = Decoder.decode
end

module Second = struct
  include Message (String) ( )

  let encoder ( ) = Encoder.encode "Hello, World!"
  let decoder     = Decoder.decode
end

let __first_against_first _ =
  assert_equal (First.decoder (First.encoder ( ))) "Hello, OCaml!"

let __second_against_second _ =
  assert_equal (Second.decoder (Second.encoder ( ))) "Hello, World!"

let suite = "applicative-suite" >::: [
  "first-against-first"   >:: __first_against_first;
  "second-against-second" >:: __second_against_second
]

let _ =
    run_test_tt_main suite

(* END *)
