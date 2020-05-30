open Plebeia.Internal
open Result
open Test_utils
open Cursor

module RS = Random.State

module Dumb = Dumb

let dump_cursor c =
  let Cursor (_, n, context) = c in
  to_file ~file:"plebeia.dot" @@ Debug.dot_of_cursor c;
  to_file ~file:"plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n
  
let () = test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ create_subtree c (path "LLL") in
  let c, () = ok_or_fail @@ Deep.deep ~go_up:true ~create_subtrees:true c [path "LLL"; path "RRR"] (fun cur seg -> upsert cur seg (value "LLL/RRR") >>| fun c -> (c, ())) in
  let Cursor (trail, _, _) = c in
  if trail <> _Top then begin
    Format.eprintf "deep strange return cursor: %s@." (String.concat "/" @@ List.map Segment.to_string @@ segs_of_trail trail);
    assert false
  end;
  let c = ok_or_fail @@ Deep.copy ~create_subtrees:true c [path "LLL"] [path "RRR"] in
  save_cursor_to_dot "copy.dot" c;
  let v = snd @@ ok_or_fail @@ Deep.get_value c [path "RRR"; path "RRR"] in
  assert (v = (value "LLL/RRR"));
  (* try to create a loop *)
  let () = must_fail @@ Deep.copy ~create_subtrees:false c [] [path "LLR"] in
  
  ignore c
