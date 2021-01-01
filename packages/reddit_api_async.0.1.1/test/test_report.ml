open! Core
open! Async
open! Import

let%expect_test "report" =
  with_cassette "report" ~f:(fun connection ->
      let target = `Link (Thing.Link.Id.of_string "hony5b") in
      let%bind () =
        Connection.call_exn connection (Api.report () ~target ~reason:"Test report")
      in
      [%expect {| |}];
      return ())
;;
