open! Core
open! Async
module Expect_test_config = Expect_test_config_with_unit_expect
include Reddit_api_async

let () = Time_ns.set_sexp_zone Time_ns.Zone.utc

let with_cassette cassette_name ~f =
  let credentials =
    match Sys.getenv "CREDENTIALS" with
    | Some credential_path ->
      Sexp.load_sexp_conv_exn credential_path [%of_sexp: Connection.Credentials.t]
    | None ->
      { username = "TEST_USERNAME"
      ; password = "TEST_PASSWORD"
      ; client_id = "TEST_CLIENT_ID"
      ; client_secret = "TEST_CLIENT_SECRET"
      }
  in
  let filename = "cassettes" ^/ sprintf "%s.sexp" cassette_name in
  Connection.For_testing.with_cassette filename ~credentials ~f
;;
