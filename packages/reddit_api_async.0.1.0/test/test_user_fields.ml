open! Core
open! Async
open! Import

let%expect_test "user_fields" =
  with_cassette "user_fields" ~f:(fun connection ->
      let%bind user =
        Connection.call_exn
          connection
          (Api.about_user ~username:(Username.of_string "spez") ())
      in
      print_s [%sexp (Thing.User.name user : Username.t)];
      [%expect {| spez |}];
      print_s [%sexp (Thing.User.creation_time user : Time_ns.t)];
      [%expect {| (2005-06-06 04:00:00.000000000Z) |}];
      print_s [%sexp (Thing.User.link_karma user : int)];
      [%expect {| 138988 |}];
      print_s [%sexp (Thing.User.comment_karma user : int)];
      [%expect {| 743899 |}];
      print_s [%sexp (Thing.User.awarder_karma user : int)];
      [%expect {| 625 |}];
      print_s [%sexp (Thing.User.awardee_karma user : int)];
      [%expect {| 62329 |}];
      print_s [%sexp (Thing.User.total_karma user : int)];
      [%expect {| 945841 |}];
      print_s
        [%sexp (Thing.User.subreddit user |> Thing.Subreddit.name : Subreddit_name.t)];
      [%expect {| u_spez |}];
      return ())
;;
