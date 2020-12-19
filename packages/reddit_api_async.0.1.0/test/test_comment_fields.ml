open! Core
open! Async
open! Import

let%expect_test "comment_fields" =
  with_cassette "comment_fields" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "hle3h4" in
      let%bind ({ comment_forest; _ } : Comment_response.t) =
        Connection.call_exn connection (Api.comments () ~link)
      in
      let first_comment =
        match List.hd_exn comment_forest with
        | `Comment comment -> comment
        | _ -> assert false
      in
      let keys_from_comment_page = Thing.Comment.field_map first_comment |> Map.key_set in
      let comment_id = Thing.Comment.id first_comment in
      let%bind keys_from_info_page =
        match%bind
          Connection.call_exn connection (Api.info (Id [ `Comment comment_id ]) ())
          >>| List.hd_exn
        with
        | `Comment comment -> return (Thing.Comment.field_map comment |> Map.key_set)
        | _ -> assert false
      in
      let diff =
        Set.symmetric_diff keys_from_comment_page keys_from_info_page |> Sequence.to_list
      in
      print_s [%sexp (diff : (string, string) Either.t list)];
      [%expect {| ((First depth)) |}];
      print_s [%sexp (Thing.Comment.depth first_comment : int option)];
      [%expect {| (0) |}];
      print_s
        [%sexp
          (Thing.Comment.body first_comment |> String.split ~on:'\n' |> List.hd_exn
            : string)];
      [%expect
        {| "- Textual/HTML pretty printing: https://github.com/c-cube/printbox/" |}];
      print_s [%sexp (Thing.Comment.score first_comment : Thing.Comment.Score.t)];
      [%expect {| (Score 11) |}];
      print_s [%sexp (Thing.Comment.link first_comment : Thing.Link.Id.t)];
      [%expect {| hle3h4 |}];
      print_s [%sexp (Thing.Comment.permalink first_comment |> Uri.to_string : string)];
      [%expect
        {| https://reddit.com/r/ocaml/comments/hle3h4/ocaml_is_superbly_suited_to_defining_and/fwzc1p0/ |}];
      print_s [%sexp (keys_from_info_page : String.Set.t)];
      [%expect
        {|
        (all_awardings approved_at_utc approved_by archived associated_award author
         author_flair_background_color author_flair_css_class author_flair_richtext
         author_flair_template_id author_flair_text author_flair_text_color
         author_flair_type author_fullname author_patreon_flair author_premium
         awarders banned_at_utc banned_by body body_html can_gild can_mod_post
         collapsed collapsed_because_crowd_control collapsed_reason controversiality
         created created_utc distinguished downs edited gilded gildings id
         is_submitter likes link_id locked mod_note mod_reason_by mod_reason_title
         mod_reports name no_follow num_reports parent_id permalink removal_reason
         replies report_reasons saved score score_hidden send_replies stickied
         subreddit subreddit_id subreddit_name_prefixed subreddit_type
         top_awarded_type total_awards_received treatment_tags ups user_reports) |}];
      return ())
;;
