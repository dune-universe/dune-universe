open! Core
open! Async
open! Import

let%expect_test "create_modmail_conversation" =
  with_cassette "create_modmail_conversation" ~f:(fun connection ->
      let%bind conversation =
        Connection.call_exn
          connection
          (Api.create_modmail_conversation
             ~subject:"Test subject"
             ~body:"Test body"
             ~subreddit:(Subreddit_name.of_string "ThirdRealm")
             ~to_:(User (Username.of_string "BJO_test_user"))
             ~hide_author:false
             ())
      in
      print_s [%sexp (conversation : Modmail.Conversation.t)];
      [%expect
        {|
        ((conversation
          (O
           ((isAuto (Bool false))
            (objIds (A ((O ((id (String osvgj)) (key (String messages)))))))
            (isRepliable (Bool true)) (lastUserUpdate Null) (isInternal (Bool false))
            (lastModUpdate (String 2020-07-26T21:18:43.146061+00:00))
            (lastUpdated (String 2020-07-26T21:18:43.146061+00:00))
            (authors
             (A
              ((O
                ((isMod (Bool true)) (isAdmin (Bool false))
                 (name (String L72_Elite_Kraken)) (isOp (Bool true))
                 (isParticipant (Bool false)) (isHidden (Bool false))
                 (id (Float 71814082)) (isDeleted (Bool false)))))))
            (owner
             (O
              ((displayName (String ThirdRealm)) (type (String subreddit))
               (id (String t5_390u2)))))
            (id (String fsv44)) (isHighlighted (Bool false))
            (subject (String "Test subject")) (participant (O ())) (state (Float 0))
            (lastUnread Null) (numMessages (Float 1)))))
         (messages
          (O
           ((osvgj
             (O
              ((body
                (String
                  "<!-- SC_OFF --><div class=\"md\"><p>Test body</p>\
                 \n</div><!-- SC_ON -->"))
               (author
                (O
                 ((isMod (Bool true)) (isAdmin (Bool false))
                  (name (String L72_Elite_Kraken)) (isOp (Bool true))
                  (isParticipant (Bool false)) (isHidden (Bool false))
                  (id (Float 71814082)) (isDeleted (Bool false)))))
               (isInternal (Bool false))
               (date (String 2020-07-26T21:18:43.146061+00:00))
               (bodyMarkdown (String "Test body")) (id (String osvgj))))))))
         (modActions (O ()))) |}];
      return ())
;;
