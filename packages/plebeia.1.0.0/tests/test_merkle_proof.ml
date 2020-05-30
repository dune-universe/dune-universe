open Plebeia.Internal
open Result
open Test_utils
open Cursor

module MP = Merkle_proof

module RS = Random.State

let root_hash cursor =
  let Cursor (_, node, context) = from_Ok @@ Cursor.go_top cursor in
  Node_hash.compute context node
  |> snd

let show_hash lh =
  Node_hash.prefix lh |> Hash.to_hex_string

let show_path path =
  String.concat " / " @@ List.map Segment.to_string path

let check_soundness segs cursor =
  match
    MP.generate cursor segs >>= fun mproof ->
    MP.validate segs mproof >>= fun hash ->
    Ok hash
  with
  | Ok hash ->
     let expected = root_hash cursor in
     if expected <> hash then begin
         let Cursor (_, node, _) = cursor in
         (Format.sprintf "!!! Merkle Proof Test Failed !!!\n Expected hash: %s\n but Exact: %s\nnode:\n%s" (show_hash expected) (show_hash hash) (Debug.string_of_node node 2))
         |> prerr_endline;
         assert false
       end
  | Error err ->
     failwith (Error.show err)

let check_encoding segs cursor =
  let open Data_encoding in
  match MP.generate cursor segs with
  | Ok mproof ->
     let bson = Bson.construct MP.encoding mproof in
     let mproof' = Bson.destruct MP.encoding bson in
     if mproof' <> mproof then
       Format.printf "Merkle Proof Encoding Test Failed.\nThe Bson\n"
  | Error err ->
     failwith (Error.show err)

let check segs cursor =
  check_soundness segs cursor;
  check_encoding segs cursor

let segment s = from_Some @@ Segment.of_string s

let test1 ctx =
  let leaf = Node.new_leaf (Value.of_string "HOGE") in
  let seg = segment "LL" in
  let ext = Node.new_extender seg leaf in
  let node = Node.new_bud (Some ext) in
  let cursor = _Cursor (_Top, node, ctx) in
  check [seg] cursor

let test2 ctx =
  let left = Node.new_leaf (Value.of_string "LEFT") in
  let right = Node.new_leaf (Value.of_string "RIGHT") in
  let internal = Node.new_internal left right in
  let prefix = segment "LL" in
  let ext = Node.new_extender prefix internal in
  let node = Node.new_bud (Some ext) in
  let cursor = _Cursor (_Top, node, ctx) in
  let seg = Segment.append prefix (segment "R") in
  check [seg] cursor

let test3 ctx =
  let seg1 = segment "RRRRR" in
  let seg1' = segment "RRRLR" in
  let seg2 = segment "LLLLL" in
  let seg3 = segment "LLRL" in
  let value1 = Value.of_string "HOGE1" in
  let value2 = Value.of_string "HOGE2" in
  let cursor =
    Cursor.empty ctx
    |> (fun c -> Cursor.create_subtree c seg1)   (* mkdir "RRRRR" *)
    >>= (fun c -> Cursor.create_subtree c seg1') (* mkdir "RRRLR" *)
    >>= (fun c -> Cursor.subtree c seg1)         (* cd "RRRRR" *)
    >>= (fun c -> Cursor.insert c seg2 value1)
    >>= (fun c -> Cursor.insert c seg3 value2)
    >>= Cursor.go_top
    |> from_Ok
  in
  check [seg1; seg2] cursor;
  check [seg1; seg3] cursor

let test4 ctx =
  let rec create store node depth =
    if depth = 0 then
      (List.rev store, node)
    else
      let seg = segment "RR" in
      let (node, _, _) =
        Node.new_extender seg node
        |> fun node -> Node.new_bud (Some node)
        |> Node_storage.commit_node ctx (Bud_cache.empty ())
      in
      create (seg :: store) node (depth - 1)
  in
  let value = Value.of_string "HOGE" in
  let (path, node) = create [] (Node.new_leaf value) 1000000 in
  let cursor = _Cursor(Cursor._Top, node, ctx) in
  check path cursor

let random_check st =
  let len = 10000 in
  test_with_context len (fun context ->
      let max_depth = 5 in
      let node = Gen.root max_depth st in
      let cursor = _Cursor(_Top, node, context) in
      let pathes = every_leaves context node in
      if pathes <> [] then
        let path = random_choice st pathes in
        check path cursor)

let () =
  let len = 10000 in
  test_with_context len (fun ctx ->
      test1 ctx;
      test2 ctx;
      test3 ctx;
      test4 ctx
    );
  let st = RS.make_self_init () in
  for _ = 0 to 100 do
    random_check st
  done;
  ()
