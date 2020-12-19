open! Core
open! Async
open! Import

let%expect_test "edit" =
  with_cassette "edit" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind comment =
        Connection.call_exn connection (Api.edit () ~id ~text:"edited text")
      in
      print_s [%sexp (Thing.Poly.fullname comment : Thing.Fullname.t)];
      [%expect {| (Comment g3krlj5) |}];
      return ())
;;

let%expect_test "save" =
  with_cassette "save" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () = Connection.call_exn connection (Api.save () ~id) in
      [%expect {| |}];
      return ())
;;

let%expect_test "unsave" =
  with_cassette "unsave" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () = Connection.call_exn connection (Api.unsave () ~id) in
      (* Unsave is idempotent *)
      let%bind () = Connection.call_exn connection (Api.unsave () ~id) in
      [%expect {| |}];
      return ())
;;

let%expect_test "send_replies" =
  with_cassette "send_replies" ~f:(fun connection ->
      let id = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () =
        Connection.call_exn connection (Api.send_replies () ~id ~enabled:true)
      in
      [%expect];
      let%bind () =
        Connection.call_exn connection (Api.send_replies () ~id ~enabled:false)
      in
      [%expect];
      return ())
;;

let%expect_test "set_contest_mode" =
  with_cassette "set_contest_mode" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "hofd3k" in
      let%bind () =
        Connection.call_exn connection (Api.set_contest_mode () ~link ~enabled:true)
      in
      let%bind () =
        Connection.call_exn connection (Api.set_contest_mode () ~link ~enabled:false)
      in
      [%expect];
      return ())
;;

let%expect_test "spoiler" =
  with_cassette "spoiler" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "hofd3k" in
      let%bind () = Connection.call_exn connection (Api.spoiler () ~link) in
      [%expect];
      return ())
;;

let%expect_test "unspoiler" =
  with_cassette "unspoiler" ~f:(fun connection ->
      let link = Thing.Link.Id.of_string "hofd3k" in
      let%bind () = Connection.call_exn connection (Api.unspoiler () ~link) in
      [%expect];
      return ())
;;

let%expect_test "vote" =
  with_cassette "vote" ~f:(fun connection ->
      let target = `Comment (Thing.Comment.Id.of_string "g3krlj5") in
      let%bind () =
        Connection.call_exn connection (Api.vote () ~target ~direction:Down)
      in
      let%bind () =
        Connection.call_exn connection (Api.vote () ~target ~direction:Neutral)
      in
      let%bind () = Connection.call_exn connection (Api.vote () ~target ~direction:Up) in
      [%expect];
      return ())
;;
