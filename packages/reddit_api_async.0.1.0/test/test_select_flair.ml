open! Core
open! Async
open! Import

let%expect_test "select_flair" =
  with_cassette "select_flair" ~f:(fun connection ->
      let subreddit = Subreddit_name.of_string "ThirdRealm" in
      let link = Thing.Link.Id.of_string "hmjghn" in
      let flair_template_id = Uuid.of_string "6c5ea4bc-c16c-11ea-9a01-0ea60516144b" in
      let%bind () =
        Connection.call_exn
          connection
          (Api.select_flair ~flair_template_id () ~subreddit ~target:(Link link))
      in
      [%expect {| |}];
      return ())
;;
