open! Core
open! Async
open! Import
open Relationship

let subreddit = Subreddit_name.of_string "ThirdRealm"

let%expect_test "banned" =
  with_cassette "banned" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.banned () ~subreddit)
        >>| Listing.children
        >>| List.iter ~f:(fun ban ->
                print_s
                  [%sexp
                    { relationship_id : Ban.Id.t = Ban.relationship_id ban
                    ; username : Username.t = Ban.username ban
                    ; user_id : Thing.User.Id.t = Ban.user_id ban
                    ; note : string = Ban.note ban
                    ; days_left : int option = Ban.days_left ban
                    ; date : Time_ns.t = Ban.date ban
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_26zyq4m) (username ketralnis) (user_id nn0q)
           (note "blah blah blah") (days_left (2))
           (date (2020-09-17 12:50:33.000000000Z)))
          ((relationship_id rb_26zynhm) (username spez) (user_id 1w72)
           (note "blah blah blah: Capricious ban") (days_left ())
           (date (2020-09-17 12:49:32.000000000Z))) |}];
      return ())
;;

let%expect_test "muted" =
  with_cassette "muted" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.muted () ~subreddit)
        >>| Listing.children
        >>| List.iter ~f:(fun mute ->
                print_s
                  [%sexp
                    { relationship_id : Mute.Id.t = Mute.relationship_id mute
                    ; username : Username.t = Mute.username mute
                    ; user_id : Thing.User.Id.t = Mute.user_id mute
                    ; date : Time_ns.t = Mute.date mute
                    }])
      in
      [%expect
        {|
          ((relationship_id Mute_c00caa20-fac7-11ea-a7ae-22f314d2d040)
           (username BJO_test_user) (user_id xw1ym)
           (date (2020-09-19 22:30:41.000000000Z))) |}];
      return ())
;;

let%expect_test "wiki_banned" =
  with_cassette "wiki_banned" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.wiki_banned () ~subreddit)
        >>| Listing.children
        >>| List.iter ~f:(fun ban ->
                print_s
                  [%sexp
                    { relationship_id : Ban.Id.t = Ban.relationship_id ban
                    ; username : Username.t = Ban.username ban
                    ; user_id : Thing.User.Id.t = Ban.user_id ban
                    ; note : string = Ban.note ban
                    ; days_left : int option = Ban.days_left ban
                    ; date : Time_ns.t = Ban.date ban
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_276wzg3) (username BJO_test_user) (user_id xw1ym)
           (note bar) (days_left (2)) (date (2020-09-19 22:39:05.000000000Z))) |}];
      return ())
;;

let%expect_test "contributors" =
  with_cassette "contributors" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.contributors () ~subreddit)
        >>| Listing.children
        >>| List.iter ~f:(fun contributor ->
                print_s
                  [%sexp
                    { relationship_id : Contributor.Id.t =
                        Contributor.relationship_id contributor
                    ; username : Username.t = Contributor.username contributor
                    ; user_id : Thing.User.Id.t = Contributor.user_id contributor
                    ; date : Time_ns.t = Contributor.date contributor
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_rktlv8) (username BJO_test_user) (user_id xw1ym)
           (date (2017-10-17 22:31:49.000000000Z)))
          ((relationship_id rb_g6xsft) (username BJO_test_mod) (user_id xw27h)
           (date (2016-06-05 02:12:22.000000000Z))) |}];
      return ())
;;

let%expect_test "wiki_contributors" =
  with_cassette "wiki_contributors" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.wiki_contributors () ~subreddit)
        >>| Listing.children
        >>| List.iter ~f:(fun contributor ->
                print_s
                  [%sexp
                    { relationship_id : Contributor.Id.t =
                        Contributor.relationship_id contributor
                    ; username : Username.t = Contributor.username contributor
                    ; user_id : Thing.User.Id.t = Contributor.user_id contributor
                    ; date : Time_ns.t = Contributor.date contributor
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_278zt5u) (username L72_Elite_Kraken) (user_id 16r83m)
           (date (2020-09-20 16:45:27.000000000Z))) |}];
      return ())
;;

let%expect_test "moderators" =
  with_cassette "moderators" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.moderators () ~subreddit:(Subreddit_name.of_string "redditdev"))
        >>| Listing.children
        >>| List.iter ~f:(fun moderator ->
                print_s
                  [%sexp
                    { relationship_id : Moderator.Id.t =
                        Moderator.relationship_id moderator
                    ; username : Username.t = Moderator.username moderator
                    ; user_id : Thing.User.Id.t = Moderator.user_id moderator
                    ; date : Time_ns.t = Moderator.date moderator
                    ; permissions : string list = Moderator.permissions moderator
                    ; flair_text : string option = Moderator.flair_text moderator
                    ; flair_css_class : string option =
                        Moderator.flair_css_class moderator
                    }])
      in
      [%expect
        {|
          ((relationship_id rb_l5k8) (username ketralnis) (user_id nn0q)
           (date (2008-06-18 15:51:21.000000000Z)) (permissions (all))
           (flair_text ("reddit admin")) (flair_css_class ()))
          ((relationship_id rb_l5ka) (username spez) (user_id 1w72)
           (date (2008-06-18 15:51:23.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_l5kb) (username jedberg) (user_id 1wnj)
           (date (2008-06-18 15:51:25.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_l5kc) (username kn0thing) (user_id 1wh0)
           (date (2008-06-18 15:51:27.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_l5kj) (username KeyserSosa) (user_id 1wjm)
           (date (2008-06-18 15:51:37.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_eb0p7) (username spladug) (user_id 3imtq)
           (date (2011-04-18 04:56:51.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_ggr26) (username chromakode) (user_id 7onf)
           (date (2011-06-14 07:24:34.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_59vvph) (username kemitche) (user_id 3jo4g)
           (date (2014-03-14 21:17:50.000000000Z)) (permissions (all))
           (flair_text ("Reddit Admin")) (flair_css_class ()))
          ((relationship_id rb_lue6xm) (username bboe) (user_id 3pz6e)
           (date (2017-01-31 21:09:49.000000000Z)) (permissions (all))
           (flair_text ("PRAW Author")) (flair_css_class ()))
          ((relationship_id rb_os2pwe) (username taylorkline) (user_id 13jrwt)
           (date (2017-05-25 00:42:54.000000000Z)) (permissions (wiki flair))
           (flair_text ("Bot Developer")) (flair_css_class ()))
          ((relationship_id rb_qv0r5h) (username bsimpson) (user_id 3c639)
           (date (2017-09-07 17:30:32.000000000Z)) (permissions (all)) (flair_text ())
           (flair_css_class ()))
          ((relationship_id rb_xovcgy) (username Stuck_In_the_Matrix) (user_id bk1iz)
           (date (2018-06-22 06:23:11.000000000Z))
           (permissions (posts access mail config flair))
           (flair_text ("Pushshift.io data scientist")) (flair_css_class ())) |}];
      return ())
;;

let%expect_test "delete_subreddit_banner" =
  with_cassette "delete_subreddit_banner" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.delete_subreddit_image () ~subreddit ~image:Mobile_banner)
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "delete_subreddit_header" =
  with_cassette "delete_subreddit_header" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.delete_subreddit_image () ~subreddit ~image:Header)
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "delete_subreddit_icon" =
  with_cassette "delete_subreddit_icon" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.delete_subreddit_image () ~subreddit ~image:Mobile_icon)
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "delete_subreddit_image" =
  with_cassette "delete_subreddit_image" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.delete_subreddit_image
             ()
             ~subreddit
             ~image:(Stylesheet_image { name = "leviroth" }))
      in
      [%expect {| |}];
      return ())
;;

let%expect_test "search_subreddits_by_name" =
  with_cassette "search_subreddits_by_name" ~f:(fun connection ->
      let%bind subreddits =
        Connection.call_exn connection (Api.search_subreddits_by_name () ~query:"python")
      in
      print_s [%sexp (subreddits : Subreddit_name.t list)];
      [%expect
        {|
          (Python pythontips PythonProjects2 pythonforengineers python_netsec
           PythonJobs pythoncoding PythonGUI PythonNoobs pythonclass) |}];
      return ())
;;

let%expect_test "submit_text" =
  with_cassette "submit_text" ~f:(fun connection ->
      let%bind submit_text =
        Connection.call_exn
          connection
          (Api.submit_text () ~subreddit:(Subreddit_name.of_string "philosophy"))
      in
      List.iter [ `markdown; `HTML ] ~f:(fun markup ->
          print_s
            [%sexp
              (Submit_text.submit_text markup submit_text |> String.sub ~pos:0 ~len:80
                : string)]);
      [%expect
        {|
          "Please make sure you have read the /r/philosophy posting rules which can be foun"
          "<!-- SC_OFF --><div class=\"md\"><p>Please make sure you have read the <a href=\"/r" |}];
      return ())
;;

let%expect_test "subreddit_autocomplete" =
  with_cassette "subreddit_autocomplete" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.subreddit_autocomplete () ~query:"python")
        >>| Listing.children
        >>| List.iter ~f:(fun subreddit ->
                print_s [%sexp (Thing.Subreddit.name subreddit : Subreddit_name.t)])
      in
      [%expect
        {|
          Python
          pythontips
          pythoncoding
          PythonProjects2
          pythonforengineers |}];
      return ())
;;

let%expect_test "set_subreddit_stylesheet" =
  with_cassette "set_subreddit_stylesheet" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.set_subreddit_stylesheet
             ()
             ~subreddit
             ~stylesheet_contents:"body {font-family: monospace}")
      in
      [%expect];
      return ())
;;

let%expect_test "subscribe" =
  let%bind () =
    with_cassette "subscribe__by_name" ~f:(fun connection ->
        let%bind () =
          Connection.call_exn
            connection
            (Api.subscribe
               ()
               ~action:Subscribe
               ~subreddits:(By_name [ Subreddit_name.of_string "python" ]))
        in
        [%expect];
        return ())
  in
  with_cassette "subscribe__by_id" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.subscribe
             ()
             ~action:Subscribe
             ~subreddits:(By_id [ Thing.Subreddit.Id.of_string "2qh0y" ]))
      in
      [%expect];
      return ())
;;

let%expect_test "search_users" =
  with_cassette "search_users" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn connection (Api.search_users () ~query:"python")
        >>| Listing.children
        >>| List.iter ~f:(fun user -> print_s [%sexp (Thing.User.name user : Username.t)])
      in
      [%expect
        {|
          MrAstroThomas
          python_boti
          python_js
          Pythoncurtus88
          PythonGB
          Python119
          python4normies
          python_engineer
          python_noob_001
          PythonTech
          PythonMaster66677
          python |}];
      return ())
;;

let%expect_test "subreddit_settings" =
  with_cassette "subreddit_settings" ~f:(fun connection ->
      let%bind subreddit_settings =
        Connection.call_exn connection (Api.subreddit_settings () ~subreddit)
      in
      print_s
        [%sexp
          (Subreddit_settings.get_field_exn subreddit_settings "restrict_posting"
           |> Json.get_bool
            : bool)];
      [%expect {| true |}];
      return ())
;;

let%expect_test "subreddit_rules" =
  with_cassette "subreddit_rules" ~f:(fun connection ->
      let%bind rules =
        Connection.call_exn connection (Api.subreddit_rules () ~subreddit)
      in
      List.iter (Subreddit_rules.subreddit_rules rules) ~f:(fun rule ->
          let open Subreddit_rules.Rule in
          print_s
            [%sexp
              { kind : Kind.t = kind rule
              ; description : string = description rule `markdown
              ; short_name : string = short_name rule
              ; report_reason : string = report_reason rule
              ; creation_time : Time_ns.t = creation_time rule
              ; priority : int = priority rule
              }]);
      [%expect
        {|
          ((kind All) (description "Rule 1 - details") (short_name "Rule 1")
           (report_reason "Rule 1 - report reason")
           (creation_time (2016-01-26 06:41:52.000000000Z)) (priority 0))
          ((kind Link) (description "Rule 2 - details") (short_name "Rule 2")
           (report_reason "Rule 2 - report reason")
           (creation_time (2016-06-13 01:38:34.000000000Z)) (priority 1))
          ((kind Comment) (description "Rule 3 - details - with *markdown*.")
           (short_name "Rule 3") (report_reason "Rule 3 - report reason")
           (creation_time (2016-11-15 03:13:28.000000000Z)) (priority 2)) |}];
      ignore (Subreddit_rules.site_rules rules : Json.t);
      ignore (Subreddit_rules.site_rules_flow rules : Json.t);
      return ())
;;

let%expect_test "subreddit_traffic" =
  with_cassette "subreddit_traffic" ~f:(fun connection ->
      let%bind traffic =
        Connection.call_exn connection (Api.subreddit_traffic () ~subreddit)
      in
      let by_date = List.take (Subreddit_traffic.by_date traffic) 4 in
      print_s [%sexp (by_date : Subreddit_traffic.By_date.t list)];
      [%expect
        {|
          (((date 2020-10-24) (uniques 0) (pageviews 0) (subscriptions 0))
           ((date 2020-10-23) (uniques 0) (pageviews 0) (subscriptions 0))
           ((date 2020-10-22) (uniques 0) (pageviews 0) (subscriptions 0))
           ((date 2020-10-21) (uniques 1) (pageviews 7) (subscriptions 0))) |}];
      let by_month = List.take (Subreddit_traffic.by_month traffic) 4 in
      print_s [%sexp (by_month : Subreddit_traffic.By_month.t list)];
      [%expect
        {|
          (((year 2020) (month Oct) (uniques 3) (pageviews 215))
           ((year 2020) (month Sep) (uniques 3) (pageviews 114))
           ((year 2020) (month Aug) (uniques 2) (pageviews 58))
           ((year 2020) (month Jul) (uniques 5) (pageviews 154))) |}];
      let by_hour = List.take (Subreddit_traffic.by_hour traffic) 4 in
      print_s [%sexp (by_hour : Subreddit_traffic.By_hour.t list)];
      [%expect
        {|
          (((hour "2020-10-24 17:00:00Z") (uniques 0) (pageviews 0))
           ((hour "2020-10-24 16:00:00Z") (uniques 0) (pageviews 0))
           ((hour "2020-10-24 15:00:00Z") (uniques 0) (pageviews 0))
           ((hour "2020-10-24 14:00:00Z") (uniques 0) (pageviews 0))) |}];
      return ())
;;

let%expect_test "subreddit_sticky" =
  with_cassette "subreddit_sticky" ~f:(fun connection ->
      let%bind sticky_id =
        Connection.call_exn connection (Api.get_sticky ~number:2 () ~subreddit)
      in
      print_s [%sexp (sticky_id : Thing.Link.Id.t)];
      [%expect {| jhgtn4 |}];
      return ())
;;

let%expect_test "get_subreddits" =
  with_cassette "get_subreddits" ~f:(fun connection ->
      let%bind subreddits =
        Connection.call_exn connection (Api.get_subreddits () ~relationship:Moderator)
        >>| Listing.children
        >>| List.map ~f:Thing.Subreddit.name
      in
      print_s [%sexp (subreddits : Subreddit_name.t list)];
      [%expect {| (ThirdRealm u_BJO_test_user) |}];
      return ())
;;

let%expect_test "search_subreddits_by_title_and_description" =
  with_cassette "search_subreddits_by_title_and_description" ~f:(fun connection ->
      let%bind subreddits =
        Connection.call_exn
          connection
          (Api.search_subreddits_by_title_and_description () ~query:"python")
        >>| Listing.children
        >>| List.map ~f:Thing.Subreddit.name
      in
      print_s [%sexp (subreddits : Subreddit_name.t list)];
      [%expect
        {|
          (Python algotrading raspberry_pi montypython shittyprogramming vim
           MachineLearning pythontips learnpython learnprogramming snakes
           programmingcirclejerk ballpython ProgrammerTIL linux emacs programming
           datascience WTF EliteDangerous coding coolgithubprojects todayilearned
           gamedev HowToHack) |}];
      return ())
;;

let%expect_test "list_subreddits" =
  with_cassette "list_subreddits" ~f:(fun connection ->
      let%bind subreddits =
        Connection.call_exn connection (Api.list_subreddits () ~sort:Popular)
        >>| Listing.children
        >>| List.map ~f:Thing.Subreddit.name
      in
      print_s [%sexp (subreddits : Subreddit_name.t list)];
      [%expect
        {|
          (Home AskReddit PublicFreakout pics politics news worldnews funny
           NoStupidQuestions nextfuckinglevel leagueoflegends tifu interestingasfuck
           relationship_advice modernwarfare videos AnimalCrossing gaming aww
           todayilearned gtaonline Minecraft memes gifs Art) |}];
      return ())
;;
