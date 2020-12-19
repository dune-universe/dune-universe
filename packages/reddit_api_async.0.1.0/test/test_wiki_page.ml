open! Core
open! Async
open! Import

let subreddit = Subreddit_name.of_string "ThirdRealm"
let page : Wiki_page.Id.t = { subreddit = Some subreddit; page = "index" }

let%expect_test "add_wiki_editor" =
  with_cassette "add_wiki_editor" ~f:(fun connection ->
      Connection.call_exn
        connection
        (Api.add_wiki_editor () ~page ~user:(Username.of_string "L72_Elite_Kraken")))
;;

let%expect_test "remove_wiki_editor" =
  with_cassette "remove_wiki_editor" ~f:(fun connection ->
      Connection.call_exn
        connection
        (Api.remove_wiki_editor () ~page ~user:(Username.of_string "L72_Elite_Kraken")))
;;

let%expect_test "toggle_wiki_revision_visibility" =
  with_cassette "toggle_wiki_revision_visibility" ~f:(fun connection ->
      let%bind result =
        Connection.call_exn
          connection
          (Api.toggle_wiki_revision_visibility
             ()
             ~page
             ~revision:
               (Wiki_page.Revision.Id.of_string "8048c97c-52ba-11e7-ab00-0ad38c20ef7e"))
      in
      print_s [%sexp (result : [ `Became_hidden | `Became_visible ])];
      [%expect {| Became_hidden |}];
      return ())
;;

let%expect_test "revert_wiki_page" =
  with_cassette "revert_wiki_page" ~f:(fun connection ->
      Connection.call_exn
        connection
        (Api.revert_wiki_page
           ()
           ~page
           ~revision:
             (Wiki_page.Revision.Id.of_string "e4d3d130-52b9-11e7-9d0c-0e1b806ed802")))
;;

let%expect_test "wiki_page_revisions" =
  with_cassette "wiki_page_revisions" ~f:(fun connection ->
      let%bind revisions =
        Connection.call_exn
          connection
          (Api.wiki_page_revisions
             ~pagination:
               (After
                  (Listing.Page_id.of_string
                     "WikiRevision_bde92910-52b1-11e7-bf40-120ea8b0860a"))
             ~limit:3
             ()
             ~page)
        >>| Listing.children
      in
      List.iter revisions ~f:(fun revision ->
          print_s
            [%sexp
              { author : Username.t option =
                  Wiki_page.Revision.author revision |> Option.map ~f:Thing.User.name
              ; page_name : string = Wiki_page.Revision.page_name revision
              ; id : Wiki_page.Revision.Id.t = Wiki_page.Revision.id revision
              ; reason : string option = Wiki_page.Revision.reason revision
              ; timestamp : Time_ns.t = Wiki_page.Revision.timestamp revision
              ; hidden : bool = Wiki_page.Revision.hidden revision
              }]);
      [%expect
        {|
          ((author (BJO_test_mod)) (page_name index)
           (id 7b080602-52b1-11e7-9041-0ed4553efb98) (reason ())
           (timestamp (2017-06-16 16:33:08.000000000Z)) (hidden false))
          ((author (BJO_test_mod)) (page_name index)
           (id 7ae7dca6-52b1-11e7-96d7-0a0b84aef9f8) (reason ())
           (timestamp (2017-06-16 16:33:08.000000000Z)) (hidden false))
          ((author (BJO_test_mod)) (page_name index)
           (id 610979ca-52b1-11e7-ad39-0e1b806ed802) (reason ())
           (timestamp (2017-06-16 16:32:24.000000000Z)) (hidden false)) |}];
      return ())
;;

let%expect_test "wiki_discussions" =
  with_cassette "wiki_discussions" ~f:(fun connection ->
      let%bind discussions =
        Connection.call_exn connection (Api.wiki_discussions () ~page)
        >>| Listing.children
      in
      List.iter discussions ~f:(fun link ->
          print_s [%sexp (Thing.Link.id link : Thing.Link.Id.t)]);
      [%expect {| jhvugk |}];
      return ())
;;

let%expect_test "wiki_pages" =
  with_cassette "wiki_pages" ~f:(fun connection ->
      let%bind pages = Connection.call_exn connection (Api.wiki_pages ~subreddit ()) in
      print_s [%sexp (pages : string list)];
      [%expect
        {|
        (config/automoderator config/description config/sidebar config/stylesheet
         config/submit_text index praw_test_page tbsettings toolbox usernotes
         wiki/index) |}];
      return ())
;;

let%expect_test "subreddit_wiki_revisions" =
  with_cassette "subreddit_wiki_revisions" ~f:(fun connection ->
      let%bind revisions =
        Connection.call_exn
          connection
          (Api.subreddit_wiki_revisions ~subreddit ~limit:1 ())
        >>| Listing.children
      in
      List.iter revisions ~f:(fun revision ->
          print_s
            [%sexp
              { author : Username.t option =
                  Wiki_page.Revision.author revision |> Option.map ~f:Thing.User.name
              ; page_name : string = Wiki_page.Revision.page_name revision
              ; id : Wiki_page.Revision.Id.t = Wiki_page.Revision.id revision
              ; reason : string option = Wiki_page.Revision.reason revision
              ; timestamp : Time_ns.t = Wiki_page.Revision.timestamp revision
              ; hidden : bool = Wiki_page.Revision.hidden revision
              }]);
      [%expect
        {|
          ((author (BJO_test_user)) (page_name index)
           (id f36c8b31-16d6-11eb-9cbb-0e03a79c97b5) (reason ("reverted back 3 years"))
           (timestamp (2020-10-25 15:30:02.000000000Z)) (hidden false)) |}];
      return ())
;;

let%expect_test "wiki_permissions" =
  with_cassette "wiki_permissions" ~f:(fun connection ->
      let%bind permissions =
        Connection.call_exn connection (Api.wiki_permissions () ~page)
      in
      print_s
        [%sexp
          { level : Wiki_page.Permissions.Level.t =
              Wiki_page.Permissions.level permissions
          ; contributors : Username.t list =
              Wiki_page.Permissions.contributors permissions
              |> List.map ~f:Thing.User.name
          ; listed : bool = Wiki_page.Permissions.listed permissions
          }];
      [%expect
        {|
          ((level Only_approved_contributors_for_this_page)
           (contributors (L72_Elite_Kraken)) (listed true)) |}];
      return ())
;;

let%expect_test "set_wiki_permissions" =
  with_cassette "set_wiki_permissions" ~f:(fun connection ->
      let%bind permissions =
        Connection.call_exn
          connection
          (Api.set_wiki_permissions ~level:Only_moderators ~listed:true () ~page)
      in
      print_s
        [%sexp
          { level : Wiki_page.Permissions.Level.t =
              Wiki_page.Permissions.level permissions
          ; listed : bool = Wiki_page.Permissions.listed permissions
          }];
      [%expect {| ((level Only_moderators) (listed true)) |}];
      return ())
;;
