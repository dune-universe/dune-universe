open! Core
open! Async
open! Import

let%expect_test "best" =
  with_cassette "best" ~f:(fun connection ->
      let%bind listing = Connection.call_exn connection (Api.best ()) in
      let child_ids = Listing.children listing |> List.map ~f:Thing.Link.id in
      print_s [%sexp (child_ids : Thing.Link.Id.t list)];
      [%expect
        {|
        (ikj8uc ikuyq6 iksp32 ikpbi4 ikveil ikdwtu ikg3cx ikksvt ikrful iki288 ikpyqv
         ikl4g6 iknntd ikrrxo ikl05w ikn7gl iksu0i ikqhgj ikp413 ikpayi ikkxfh ikkwlk
         ikoad9 ikudf8 ikmxz9) |}];
      return ())
;;

let%expect_test "links_by_id" =
  with_cassette "links_by_id" ~f:(fun connection ->
      let links = List.map ~f:Thing.Link.Id.of_string [ "icqrut"; "ikksvt" ] in
      let%bind listing = Connection.call_exn connection (Api.links_by_id () ~links) in
      let ids = Listing.children listing |> List.map ~f:Thing.Link.id in
      print_s [%sexp (ids : Thing.Link.Id.t list)];
      [%expect {| (icqrut ikksvt) |}];
      return ())
;;

let%expect_test "random" =
  with_cassette "random" ~f:(fun connection ->
      let%bind link_id =
        Connection.call_exn
          connection
          (Api.random ~subreddit:(Subreddit_name.of_string "ocaml") ())
      in
      print_s [%sexp (link_id : Thing.Link.Id.t)];
      [%expect {| feyhbv |}];
      return ())
;;
