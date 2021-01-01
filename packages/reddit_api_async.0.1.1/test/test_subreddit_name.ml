open! Core
open! Async
open! Import

let%expect_test "Normalization" =
  let subreddit_names =
    List.map
      ~f:Subreddit_name.of_string
      [ "askphilosophy"; "r/askphilosophy"; "/r/askphilosophy"; "/r/AskPhilosophy" ]
  in
  let set = Subreddit_name.Set.of_list subreddit_names in
  print_s [%message "" (set : Subreddit_name.Set.t)];
  [%expect {| (set (askphilosophy)) |}];
  let hash_set = Subreddit_name.Hash_set.of_list subreddit_names in
  print_s [%message "" (hash_set : Subreddit_name.Hash_set.t)];
  [%expect {| (hash_set (askphilosophy)) |}];
  return ()
;;

let%expect_test "User subreddits" =
  let username = Username.of_string "spez" in
  let subreddit_name = Subreddit_name.user_subreddit username in
  print_s [%message "" (subreddit_name : Subreddit_name.t)];
  [%expect {| (subreddit_name u_spez) |}];
  return ()
;;
