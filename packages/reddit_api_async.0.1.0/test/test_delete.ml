open! Core
open! Async
open! Import

let%expect_test "delete" =
  with_cassette "delete" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3f4icy") in
      let%bind () = Connection.call_exn connection (Api.delete () ~id) in
      [%expect];
      return ())
;;
