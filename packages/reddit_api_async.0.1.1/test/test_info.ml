open! Core
open! Async
open! Import

let%expect_test "info" =
  with_cassette "info" ~f:(fun connection ->
      let%bind link =
        Connection.call_exn
          connection
          (Api.info (Id [ `Link (Thing.Link.Id.of_string "hmjd8r") ]) ())
        >>| List.hd_exn
      in
      let link =
        match link with
        | `Link link -> link
        | _ -> raise_s [%message "Unexpected response item"]
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
        ((id hmjd8r) (title "This is a title") (author (BJO_test_user))
         (creation_time (2020-07-06 23:42:12.000000000Z)) (is_stickied false)) |}];
      return ())
;;

let%expect_test "info__by_subreddit_name" =
  with_cassette "info__by_subreddit_name" ~f:(fun connection ->
      let%bind subreddits =
        Connection.call_exn
          connection
          (Api.info
             (Subreddit_name
                (List.map [ "ocaml"; "redditdev"; "python" ] ~f:Subreddit_name.of_string))
             ())
        >>| List.map ~f:(function
                | `Subreddit subreddit -> subreddit
                | _ -> raise_s [%message "Unexpected response item"])
      in
      print_s
        [%sexp (List.map subreddits ~f:Thing.Subreddit.name : Subreddit_name.t list)];
      [%expect {| (redditdev ocaml Python) |}];
      return ())
;;
