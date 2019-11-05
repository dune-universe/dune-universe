open OUnit2

module R = Shared_secret.Revocable ( )

let revoke ( ) =
  try R.revoke ( ) with R.AlreadyRevoked -> ( )

let identity value = value

let __non_revoked _ =
  let identity' = R.revocable identity in
  assert_equal 5 (identity' 5)

let __revoked _ =
  let identity'     = R.revocable identity in
  let failure       = R.RevokedReference in
  let procedure ( ) = revoke ( ); ignore (identity' 5) in
  assert_raises failure procedure

let suite = "revocable-suite" >::: [
  "non-revoked" >:: __non_revoked;
  "revoked"     >:: __revoked
]

let _ =
    run_test_tt_main suite

(* END *)
