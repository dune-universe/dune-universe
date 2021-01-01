open! Core
open! Async
open! Import

let%expect_test "submit" =
  with_cassette "submit" ~f:(fun connection ->
      let title = "Test post title" in
      let subreddit = Subreddit_name.of_string "ThirdRealm" in
      let%bind id, uri =
        Connection.call_exn
          connection
          (Api.submit () ~title ~subreddit ~kind:(Self (Markdown "This is a post body.")))
      in
      print_s
        [%message
          "Submission attributes" (id : Thing.Link.Id.t) ~uri:(Uri.to_string uri : string)];
      [%expect
        {|
        ("Submission attributes" (id hmjghn)
         (uri https://www.reddit.com/r/ThirdRealm/comments/hmjghn/test_post_title/)) |}];
      return ())
;;

let%expect_test "submit__crosspost" =
  with_cassette "submit__crosspost" ~f:(fun connection ->
      let title = "Crosspost" in
      let subreddit = Subreddit_name.of_string "ThirdRealm" in
      let%bind id, uri =
        Connection.call_exn
          connection
          (Api.submit
             ()
             ~title
             ~subreddit
             ~kind:(Crosspost (Thing.Link.Id.of_string "box80e")))
      in
      print_s
        [%message
          "Submission attributes" (id : Thing.Link.Id.t) ~uri:(Uri.to_string uri : string)];
      [%expect
        {|
        ("Submission attributes" (id ili4vc)
         (uri https://www.reddit.com/r/ThirdRealm/comments/ili4vc/crosspost/)) |}];
      return ())
;;
