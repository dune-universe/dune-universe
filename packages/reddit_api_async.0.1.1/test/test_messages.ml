open! Core
open! Async
open! Import

let message_id = Thing.Message.Id.of_string "rdjz4y"

let%expect_test "block_author" =
  with_cassette "block_author" ~f:(fun connection ->
      let id = `Message message_id in
      let%bind () = Connection.call_exn connection (Api.block_author () ~id) in
      [%expect];
      return ())
;;

let%expect_test "collapse_message" =
  with_cassette "collapse_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Connection.call_exn connection (Api.collapse_message () ~messages) in
      [%expect];
      return ())
;;

let%expect_test "uncollapse_message" =
  with_cassette "uncollapse_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () =
        Connection.call_exn connection (Api.uncollapse_message () ~messages)
      in
      [%expect];
      return ())
;;

let%expect_test "read_message" =
  with_cassette "read_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Connection.call_exn connection (Api.read_message () ~messages) in
      [%expect];
      return ())
;;

let%expect_test "unread_message" =
  with_cassette "unread_message" ~f:(fun connection ->
      let messages = [ message_id ] in
      let%bind () = Connection.call_exn connection (Api.unread_message () ~messages) in
      [%expect];
      return ())
;;

let%expect_test "compose_message" =
  with_cassette "compose_message" ~f:(fun connection ->
      let%bind () =
        Connection.call_exn
          connection
          (Api.compose_message
             ()
             ~to_:(Username.of_string "BJO_test_user")
             ~subject:"This is a message"
             ~text:"This is its body")
      in
      [%expect];
      return ())
;;

let inbox_item_fullname (item : Inbox_item.t) =
  match item with
  | Comment comment -> `Comment (Inbox_item.Comment.id comment)
  | Message message -> `Message (Thing.Message.id message)
;;

let%expect_test "inbox" =
  with_cassette "inbox" ~f:(fun connection ->
      let%bind listing =
        Connection.call_exn connection (Api.inbox ~limit:2 () ~mark_read:false)
      in
      Listing.children listing
      |> List.iter ~f:(fun thing ->
             print_s [%sexp (inbox_item_fullname thing : Thing.Fullname.t)]);
      [%expect {|
          (Comment g3u0ce8)
          (Message rdjz4y) |}];
      return ())
;;

let%expect_test "inbox" =
  with_cassette "comment_replies" ~f:(fun connection ->
      let%bind listing =
        Connection.call_exn connection (Api.comment_replies ~limit:1 () ~mark_read:false)
      in
      let comment = List.hd_exn (Listing.children listing) in
      print_s
        [%sexp
          { id : Thing.Comment.Id.t = Inbox_item.Comment.id comment
          ; body : string = Inbox_item.Comment.body comment `markdown
          ; author : Username.t option = Inbox_item.Comment.author comment
          ; subreddit : Subreddit_name.t = Inbox_item.Comment.subreddit comment
          ; creation_time : Time_ns.t = Inbox_item.Comment.creation_time comment
          ; score : int = Inbox_item.Comment.score comment
          ; parent_id : Thing.Fullname.t =
              (Inbox_item.Comment.parent_id comment :> Thing.Fullname.t)
          ; new_ : bool = Inbox_item.Comment.new_ comment
          ; type_ : Inbox_item.Comment.Type.t = Inbox_item.Comment.type_ comment
          ; link_id : Thing.Link.Id.t = Inbox_item.Comment.link_id comment
          ; link_title : string = Inbox_item.Comment.link_title comment
          ; num_comments_in_thread : int =
              Inbox_item.Comment.num_comments_in_thread comment
          }];
      [%expect
        {|
        ((id daaiwot)
         (body
          "Your comment was not up to our subreddit's standards. Please read [our posting guidelines](https://www.reddit.com/r/askphilosophy/comments/47egl3/dont_answer_questions_unless_you_have_the/) before answering questions.")
         (author (BernardJOrtcutt)) (subreddit askphilosophy)
         (creation_time (2016-11-22 03:42:47.000000000Z)) (score 2)
         (parent_id (Comment daaivi7)) (new_ false) (type_ Comment_reply)
         (link_id 5dtzvo) (link_title "What would Nietzsche think of Donald Trump?")
         (num_comments_in_thread 58)) |}];
      return ())
;;

let%expect_test "unread" =
  with_cassette "unread" ~f:(fun connection ->
      let%bind listing =
        Connection.call_exn connection (Api.unread () ~mark_read:false)
      in
      Listing.children listing
      |> List.iter ~f:(fun thing ->
             print_s [%sexp (inbox_item_fullname thing : Thing.Fullname.t)]);
      [%expect {| (Comment g3u0ce8) |}];
      return ())
;;

let%expect_test "sent" =
  with_cassette "sent" ~f:(fun connection ->
      let%bind listing = Connection.call_exn connection (Api.sent ~limit:2 ()) in
      Listing.children listing
      |> List.iter ~f:(fun message ->
             print_s [%sexp (Thing.Message.id message : Thing.Message.Id.t)]);
      [%expect {|
          rdkr3p
          rdk8sp |}];
      return ())
;;
