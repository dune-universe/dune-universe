open! Core
open! Async
open! Import

let children_of_optional_listing opt = Option.to_list opt |> List.bind ~f:Listing.children

let print_links links =
  let children = children_of_optional_listing links in
  List.iter children ~f:(fun link ->
      print_s [%sexp (Thing.Link.id link : Thing.Link.Id.t)])
;;

let print_users_and_subreddits listing =
  let children = children_of_optional_listing listing in
  List.iter children ~f:(fun thing ->
      print_s [%sexp (Thing.Poly.fullname thing : Thing.Fullname.t)])
;;

let%expect_test "search" =
  with_cassette "search" ~f:(fun connection ->
      let%bind links, users_and_subreddits =
        Connection.call_exn connection (Api.search () ~query:"ocaml")
      in
      print_links links;
      [%expect
        {|
        ihn5kn
        idh5be
        idq8tq
        ij3wf8
        igdkjr
        ig3mop
        ilz7sw
        igf0xc
        i91ov0
        hp6rdr
        igcsf1
        ic21kp
        icu8of
        iatfaf
        i31lhf
        ikksvt
        iaehta
        ij38m8
        ijygv6
        icyr41
        ia2ht5
        iiq5uh
        i6e9gz
        i4mtbv
        icqrut |}];
      print_users_and_subreddits users_and_subreddits;
      [%expect];
      return ())
;;

let%expect_test "search__subreddits" =
  with_cassette "search__subreddits" ~f:(fun connection ->
      let%bind links, users_and_subreddits =
        Connection.call_exn
          connection
          (Api.search
             ()
             ~types:(Api.Parameters.Search_type.Set.singleton Subreddit)
             ~query:"ocaml")
      in
      print_links links;
      [%expect];
      print_users_and_subreddits users_and_subreddits;
      [%expect
        {|
        (Subreddit 2qh60)
        (Subreddit 2qh36)
        (Subreddit 3fcct)
        (Subreddit 2vcmg)
        (Subreddit 2fwo)
        (Subreddit 2uiob)
        (Subreddit 3f53f)
        (Subreddit 2s7lj)
        (Subreddit 2qkey)
        (Subreddit 2qh37)
        (Subreddit 2qi8m)
        (Subreddit 2r7yd)
        (Subreddit 2qmd0)
        (Subreddit 2qhmr)
        (Subreddit 2qh1a)
        (Subreddit 2qrgl)
        (Subreddit 2r0sd)
        (Subreddit 2tex6)
        (Subreddit 2qiuk)
        (Subreddit 2qxyj)
        (Subreddit zsv0p)
        (Subreddit 2xekg)
        (Subreddit 2sslb)
        (Subreddit 3cmf4)
        (Subreddit 3cv1n) |}];
      return ())
;;

let%expect_test "search__all" =
  with_cassette "search__all" ~f:(fun connection ->
      let%bind links, users_and_subreddits =
        Connection.call_exn
          connection
          (Api.search
             ()
             ~types:Api.Parameters.Search_type.(Set.of_list all)
             ~query:"spez")
      in
      print_links links;
      [%expect
        {|
        ipiwyi
        iav0ir
        ipjm8d
        itn92x
        ippnca
        isij2o
        hil81d
        ipjw96
        ik2p2q
        iot8wt
        h7e893
        il54f5
        itl02c
        gv7mtn
        gwah8p
        hbqrl8
        im5haj
        ibwj10
        ibmnz6
        h7uh5b
        iot9mx
        ig4086 |}];
      print_users_and_subreddits users_and_subreddits;
      [%expect
        {|
        (Subreddit 2qij9)
        (User 1w72)
        (Subreddit 3glzb) |}];
      return ())
;;
