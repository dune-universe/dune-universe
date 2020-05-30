(* node storage test *)
open Plebeia.Internal
open Test_utils
open Debug
open Node

module RS = Random.State

let set_uint32 buf x v = Cstruct.LE.set_uint32 buf x @@ Stdint.Uint32.to_int32 v
let set_index buf x v = set_uint32 buf x @@ Index.to_uint32 v
let write_string s buf off len =
  let slen = String.length s in
  if slen <> len then begin Format.eprintf "write_string: %d <> %d@." slen len; assert false end;
  Cstruct.blit_from_string s 0 buf off len

let parse_test context n =
  (* reload the node and compare *)
  try
    let n' = View (Node_storage.parse_cell context.Context.storage @@ from_Some @@ index n) in
    let n' = Node_storage.load_node_fully context n' in
    if not (equal_nodes n n') then begin
      prerr_endline @@ string_of_node n 2;
      prerr_endline @@ string_of_node n' 2;
      assert false
    end
  with
  | e ->
      prerr_endline @@ string_of_node n 2;
      raise e

let random_write_parse st =
  test_with_cursor @@ fun (Cursor (_, _, context)) ->

  match RS.int st 4 with
  | 0 (* leaf *) ->
      let size = RS.int st 64 in
      let v = Value.of_string @@ random_string st size in
      let n = new_leaf v in
      let n, _i, _h = Node_storage.commit_node context (Bud_cache.empty ()) n in
      parse_test context n

  | 1 (* bud *) ->
      if RS.bool st then
        let n = new_bud None in
        let n, _, _ = Node_storage.commit_node context (Bud_cache.empty ()) n in
        parse_test context n
      else
        let size = RS.int st 64 in
        let v = Value.of_string @@ random_string st size in
        let n = new_bud (Some (new_extender (path "L") (new_leaf v))) in
        let n, _, _ = Node_storage.commit_node context (Bud_cache.empty ()) n in
        parse_test context n

  | 2 (* internal *) ->
      let right_referred = RS.bool st in
      let n1, _, _ =
        Node_storage.commit_node context (Bud_cache.empty ()) @@
        let size = RS.int st 16 in
        new_leaf @@ Value.of_string @@ random_string st size
      in
      let n2 =
        let size = RS.int st 16 in
        new_leaf @@ Value.of_string @@ random_string st size
      in
      let n =
        if right_referred then
          new_internal n2 n1
        else
          new_internal n1 n2
      in
      let n, _, _ = Node_storage.commit_node context (Bud_cache.empty ()) n in
      parse_test context n

  | 3 (* extender *) ->
      let seg = random_segment st in (* may be a big segment! *)
      let n' =
        let size = RS.int st 16 in
        new_leaf @@ Value.of_string @@ random_string st size
      in
      let n = new_extender seg n' in
      let n, _, _ = Node_storage.commit_node context (Bud_cache.empty ()) n in
      parse_test context n

  | _ -> assert false

let () =
  let st = Random.State.make_self_init () in
  for _ = 1 to 10000 do random_write_parse st done
