open OUnit2
open Shared_secret

module First = struct
  let (token, revoker) = Token.create ( )

  let sealer ( ) = Box.Sealer.seal token "Hello, OCaml!"
  let unsealer   = Box.Unsealer.unseal token
end

module Second = struct
  let (token, _) = Token.create ( )

  let sealer ( ) = Box.Sealer.seal token "Hello, World!"
  let unsealer   = Box.Unsealer.unseal token
end

let __first_against_first _ =
  assert_equal (First.unsealer (First.sealer ( ))) "Hello, OCaml!"

let __second_against_first _ =
  let failure       = Box.InvalidToken in
  let procedure ( ) = Second.unsealer (First.sealer ( )) in
  assert_raises failure procedure

let __first_against_second _ =
  let failure       = Box.InvalidToken in
  let procedure ( ) = First.unsealer (Second.sealer ( )) in
  assert_raises failure procedure

let __second_against_second _ =
  assert_equal (Second.unsealer (Second.sealer ( ))) "Hello, World!"

let sealed = First.sealer ( )

let revoke ( ) =
  try Token.revoke First.revoker with Token.AlreadyRevoked -> ( )

let __first_cant_seal _ =
  let failure       = Token.RevokedToken in
  let procedure ( ) = revoke ( ); First.sealer ( ) in
  assert_raises failure procedure

let __first_cant_unseal _ =
  let failure       = Token.RevokedToken in
  let procedure ( ) = revoke ( ); First.unsealer sealed in
  assert_raises failure procedure

let suite = "generic-suite" >::: [
  "first-against-first"   >:: __first_against_first;
  "second-against-first"  >:: __second_against_first;
  "first-against-second"  >:: __first_against_second;
  "second-against-second" >:: __second_against_second;
  "first-cant-seal"       >:: __first_cant_seal;
  "first-cant-unseal"     >:: __first_cant_unseal
]

let _ =
    run_test_tt_main suite

(* END *)
