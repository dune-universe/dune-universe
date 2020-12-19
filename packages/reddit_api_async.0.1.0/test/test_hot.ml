open! Core
open! Async
open! Import

let%expect_test "hot" =
  with_cassette "hot" ~f:(fun connection ->
      let%bind link =
        Connection.call_exn
          connection
          (Api.hot ~limit:1 ~subreddit:(Subreddit_name.of_string "ThirdRealm") ())
        >>| Listing.children
        >>| List.hd_exn
      in
      print_s
        [%sexp
          { id : Thing.Link.Id.t = Thing.Link.id link
          ; title : string = Thing.Link.title link
          ; author : Username.t option = Thing.Link.author link
          ; creation_time : Time_ns.t = Thing.Link.creation_time link
          ; is_stickied : bool = Thing.Link.is_stickied link
          }];
      [%expect
        {|
        ((id fa5dg9)
         (title "/r/thirdrealm Open Discussion Thread | February 26, 2020")
         (author (BernardJOrtcutt)) (creation_time (2020-02-27 02:55:31.000000000Z))
         (is_stickied true)) |}];
      return ())
;;

let%expect_test "hot__multiple_subreddits" =
  with_cassette "hot__multiple_subreddits" ~f:(fun connection ->
      let subreddit =
        List.map [ "aww"; "programming" ] ~f:Subreddit_name.of_string
        |> Subreddit_name.combine
      in
      let%bind links =
        Connection.call_exn connection (Api.hot ~limit:10 ~subreddit ())
        >>| Listing.children
      in
      List.iter links ~f:(fun link ->
          print_s
            [%sexp
              { subreddit : Subreddit_name.t = Thing.Link.subreddit link
              ; id : Thing.Link.Id.t = Thing.Link.id link
              }]);
      [%expect
        {|
        ((subreddit aww) (id j0q3oj))
        ((subreddit programming) (id j0oriz))
        ((subreddit aww) (id j0omwl))
        ((subreddit programming) (id j06gd7))
        ((subreddit aww) (id j0mb8j))
        ((subreddit aww) (id j0n001))
        ((subreddit aww) (id j0nuzp))
        ((subreddit aww) (id j0og2d))
        ((subreddit aww) (id j0li9w))
        ((subreddit aww) (id j0njcb)) |}];
      return ())
;;
