open! Core
open! Async
open! Import

let%expect_test "friends" =
  with_cassette "friends" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Api.friends ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Float 1598656681)) (id (String t2_1w72)) (name (String spez))
          (rel_id (String r9_1voxr1)))) |}];
      return ())
;;

let%expect_test "blocked" =
  with_cassette "blocked" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Api.blocked ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Float 1598788910)) (id (String t2_nn0q)) (name (String ketralnis))
          (rel_id (String r9_1vt16j)))) |}];
      return ())
;;

let%expect_test "messaging" =
  with_cassette "messaging" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Api.messaging ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Float 1598789198)) (id (String t2_1w72)) (name (String spez))
          (rel_id (String r9_1vt1em)))) |}];
      return ())
;;

let%expect_test "trusted" =
  with_cassette "trusted" ~f:(fun connection ->
      let%bind body = Connection.call_exn connection (Api.trusted ()) in
      print_s [%sexp (body : User_list.t)];
      [%expect
        {|
        (((date (Float 1598789198)) (id (String t2_1w72)) (name (String spez))
          (rel_id (String r9_1vt1em)))) |}];
      return ())
;;
