open! Core
open! Async
open! Import

let%expect_test "me" =
  with_cassette "me" ~f:(fun connection ->
      let%bind me = Connection.call_exn connection (Api.me ()) in
      let id = Thing.User.id me in
      print_s [%sexp (id : Thing.User.Id.t)];
      [%expect {| 16r83m |}];
      return ())
;;
