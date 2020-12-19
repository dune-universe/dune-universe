open! Core
open! Async
open! Import

let%expect_test "subreddit_fields" =
  with_cassette "subreddit_fields" ~f:(fun connection ->
      let%bind subreddit =
        Connection.call_exn
          connection
          (Api.about_subreddit ~subreddit:(Subreddit_name.of_string "ocaml") ())
      in
      print_s [%sexp (Thing.Subreddit.name subreddit : Subreddit_name.t)];
      [%expect {| ocaml |}];
      print_s [%sexp (Thing.Subreddit.title subreddit : string)];
      [%expect {| "let reddit = OCaml;;" |}];
      print_s [%sexp (String.prefix (Thing.Subreddit.description subreddit) 80 : string)];
      [%expect
        {|
        "[OCaml](http://ocaml.org/) is a statically typed functional programming language" |}];
      print_s [%sexp (Thing.Subreddit.subscribers subreddit : int)];
      [%expect {| 7554 |}];
      print_s [%sexp (Thing.Subreddit.active_users subreddit : int)];
      [%expect {| 12 |}];
      print_s [%sexp (Thing.Subreddit.creation_time subreddit : Time_ns.t)];
      [%expect {| (2008-01-25 13:24:41.000000000Z) |}];
      return ())
;;
