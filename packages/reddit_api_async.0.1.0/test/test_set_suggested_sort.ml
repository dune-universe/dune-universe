open! Core
open! Async
open! Import

let%expect_test "set_suggested_sort" =
  with_cassette "set_suggested_sort" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "hmjghn" in
      let%bind () =
        Connection.call_exn connection (Api.set_suggested_sort () ~sort:(Some New) ~link)
      in
      [%expect {| |}];
      return ())
;;
