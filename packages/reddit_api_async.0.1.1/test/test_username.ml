open! Core
open! Async
open! Import

let%expect_test "Normalization" =
  let usernames =
    List.map ~f:Username.of_string [ "spez"; "u/spez"; "/u/spez"; "/u/SPEZ" ]
  in
  let set = Username.Set.of_list usernames in
  print_s [%message "" (set : Username.Set.t)];
  [%expect {| (set (spez)) |}];
  let hash_set = Username.Hash_set.of_list usernames in
  print_s [%message "" (hash_set : Username.Hash_set.t)];
  [%expect {| (hash_set (spez)) |}];
  return ()
;;
