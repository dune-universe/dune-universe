open! Core
open! Async
open! Import

let%expect_test "set_subreddit_sticky" =
  with_cassette "set_subreddit_sticky" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "f7vspj" in
      let%bind () =
        Connection.call_exn
          connection
          (Api.set_subreddit_sticky () ~link ~sticky_state:(Sticky { slot = Some 2 }))
      in
      [%expect];
      let%bind () =
        Connection.call_exn
          connection
          (Api.set_subreddit_sticky () ~link ~sticky_state:Unsticky)
      in
      [%expect];
      return ())
;;
