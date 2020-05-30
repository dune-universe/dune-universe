open Plebeia.Internal
(* open Result *)
open Test_utils
open Cursor
open Node

module RS = Random.State

module Debug = Debug

module Dumb = Dumb

let commit_and_load c =
  let Cursor (tr, _n, context), i, _ = Cursor_storage.commit_top_cursor (Bud_cache.empty ()) c in
  let n = View (load_node context i Not_Extender) in
  _Cursor (tr, n, context)

let () = 
  test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ upsert c (path "LLRR") (value "LLRR") in
  let c = ok_or_fail @@ upsert c (path "LLLLLL") (value "LLLLLL") in
  let c = ok_or_fail @@ upsert c (path "LLLLRR") (value "LLLLRR") in
  save_cursor_to_dot "ext1.dot" c;
  let c = commit_and_load c in
  save_cursor_to_dot "ext2.dot" c;
  let c = ok_or_fail @@ delete c (path "LLRR") in (* We got here Fatal error: exception (Failure "Extender: cannot have Disk with Maybe_Extender"), because of a bug of remove_up *)
  save_cursor_to_dot "ext3.dot" c;
  ignore c
