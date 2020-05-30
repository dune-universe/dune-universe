(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
open Node
open Storage
module C = Xcstruct

(** node storage.
    See Layout.md for the format *)

exception LoadFailure of Error.t

let zero_sized_leaf_index = Index.zero
let zero_sized_leaf_hash = Value.of_string "" |> Node_hash.of_leaf
let zero_sized_leaf_short_hash = Node_hash.prefix zero_sized_leaf_hash

let rec parse_cell storage i =
  let buf = make_buf storage i in
  let tag = C.get_index buf 28 in
  let tag_int32 = Index.to_int32 tag in (* easier to match *)
  if i = zero_sized_leaf_index then
    let h = zero_sized_leaf_short_hash in
    let v = Value.of_string "" in
    _Leaf (v, Indexed i, Hashed h)
  else begin
  match tag_int32 with
  | -256l -> (* empty bud *)
      _Bud (None, Indexed i, Hashed Node_hash.(prefix @@ of_bud None))

  | -255l -> (* leaf whose value is in Plebeia *)
      let h = C.get_hash buf 0 in
      let v = Value.of_string @@ Chunk.read storage @@ Index.pred i in
      _Leaf (v, Indexed i, Hashed h)

  | -254l -> (* linked *)
      let i' = C.get_index buf 24 in
      parse_cell storage i'
      
  | x when -32l <= x && x <= -1l -> (* leaf whose value is in the previous cell *)
      let l = - Int32.to_int x in (* 1 to 32 *)
      let h = C.get_hash buf 0 in
      let buf = make_buf storage (Index.pred i) in
      let v = Value.of_string @@ C.copy buf 0 l in
      _Leaf (v, Indexed i, Hashed h)

  | x when -64l <= x && x <= -33l -> (* leaf whose value is in the 2 previous cells *)
      let l = - Int32.to_int x in (* 33 to 64 *)
      let h = C.get_hash buf 0 in
      let buf = make_buf2 storage (Index.pred @@ Index.pred i) in
      let v = Value.of_string @@ C.copy buf 0 l in
      _Leaf (v, Indexed i, Hashed h)

  | x when -256l <= x && x <= -1l -> assert false

  | _ -> 
      let s_224 = C.copy buf 0 28 in
      let last_byte = Char.code @@ String.unsafe_get s_224 27 in
      match last_byte land 0x03 with
      | 1 -> (* extender *)
          (* extender  |....| |<- segment ->|6bits|01| |<- the index of the child ->| *)
          let i' = C.get_index buf 28 in
          let v = parse_cell storage i' in
          let cells_extra = last_byte lsr 2 in
          let rec f st n i = 
            if n = 0 then st
            else
              let buf = make_buf storage i in
              let s = C.copy buf 0 32 in
              f (s::st) (n-1) (Index.pred i)
          in
          let ss = f [String.sub s_224 0 27] cells_extra (Index.pred i) in
          let seg = Segment.decode @@ String.concat "" ss in
          let h = match hash_of_view v with
            | None -> assert false
            | Some h -> h
          in
          _Extender (seg, View v, Indexed i, Hashed h)
      | 3 -> (* non empty bud *)
          let s = C.copy buf 0 28 (* 224bits *) in
          let h = Hash.of_string s in
          let i' = C.get_index buf 28 in
          let v = parse_cell storage i' in
          (* We cannot have Disk to check the invariants *)
          _Bud (Some (View v), Indexed i, Hashed h)
          
      | 0 | 2 -> (* internal *)
          let s_0_215 = C.copy buf 0 27 (* 216bits *) in
          let c_216_223, refer_to_right = 
            let c = Char.code @@ C.get_char buf 27 in
            (Char.chr (c land 0xfc), (c land 2) = 2)
          in
          let h = Hash.of_string (s_0_215 ^ String.make 1 c_216_223) in
          let i' = C.get_index buf 28 in
          (* Because of the link, Index.pred i may not the index of the child! 
          *)
          let j = 
            let i_pred = Index.pred i in
            let buf = make_buf storage i_pred in
            let tag = C.get_index buf 28 in
            let tag_int32 = Index.to_int32 tag in (* easier to match *)
            if tag_int32 = -254l (* it is a link *) then C.get_index buf 24
            else i_pred
          in
          if refer_to_right then
            _Internal (Disk (j, Maybe_Extender), Disk (i', Maybe_Extender), Indexed i, Hashed h)
          else
            _Internal (Disk (i', Maybe_Extender), Disk(j, Maybe_Extender), Indexed i, Hashed h)
      | _ -> assert false
    end

let parse_cell storage i = 
  let v = parse_cell storage i in
  assert (Node.index_of_view v = Some i); (* 2019-09-30, we had a bug in parse_cell, around the handling of the link! *)
  v

let load_node context (index : Index.t) (ewit:extender_witness) : view = 
  let storage = context.Context.storage in
  let v = parse_cell storage index in
  Stat.incr_loaded_nodes context.Context.stat;
  match ewit, v with
  | Is_Extender, Extender _ -> v
  | Is_Extender, _ -> assert false (* better report *)
  | Maybe_Extender, Extender _ -> v
  | Not_Extender, Extender _ -> assert false (* better report *)
  | Not_Extender, _ -> v
  | Maybe_Extender, _ -> v

let () = load_node_ref := load_node

(* XXX non tail recursive *)
let rec load_node_fully context n =
  let v = match n with
    | Disk (i, ewit) -> load_node context i ewit
    | View v -> v
  in
  match v with
  | Leaf _ -> View v
  | Bud (None, _, _) -> View v
  | Bud (Some n, i, h) ->
      let n = load_node_fully context n in
      View (_Bud (Some n, i, h))
  | Internal (n1, n2, i, h) ->
      let n1 = load_node_fully context n1 in
      let n2 = load_node_fully context n2 in
      View (_Internal (n1, n2, i, h))
  | Extender (seg, n, i, h) ->
      let n = load_node_fully context n in
      View (_Extender (seg, n, i, h))

(* index 32 bits (4294967296)
   block 32 bytes 
   max size of the storage 137_438_953_472 =~ 130Gb
*)
let index n = match index n with
  | Some i -> i
  | None -> assert false

let bud_first_28 = String.make 28 '\255'
let zero_24 = String.make 24 '\000'

(* XXX The code assumes the only one writer. 
   If we change it with multiple writers, beware of the indices.
   We must allocate all the indices at once for each node.
*)

let check_mode storage =
  if Storage.mode storage = Storage.Reader then invalid_arg "Reader cannot write"
   
let write_small_leaf storage v =
  check_mode storage;
  let len = Value.length v in
  assert (1 <= len && len <= 32);
  let i = Storage.new_index storage in
  let buf = make_buf storage i in
  C.write_string (Value.to_string v) buf 0 len

let write_medium_leaf storage v =
  check_mode storage;
  let len = Value.length v in
  assert (33 <= len && len <= 64);
  let i = Storage.new_indices storage 2 in
  let buf = make_buf2 storage i in
  C.write_string (Value.to_string v) buf 0 len

let write_large_leaf_to_plebeia storage v =
  check_mode storage;
  ignore @@ Storage.Chunk.write storage (Value.to_string v)

let write_leaf context v nh =
  check_mode context.Context.storage;
  (* contents are ALREADY written *)
  let storage = context.Context.storage in
  let h = Node_hash.prefix nh in
  let i = Storage.new_index storage in
  let len = Value.length v in
  if len <= 64 then begin
    let buf = make_buf storage i in
    C.write_string (Hash.to_string h) buf 0 28;
    C.set_index buf 28 (Index.of_int (-len)) (* 1 => -1  64 -> -64 *)
  end else begin
    let k = -255l in
    let buf = make_buf storage i in
    let h = Hash.to_string h in
    C.write_string h buf 0 28;
    C.set_index buf 28 (Index.of_int32 k)
  end;
  Stat.incr_written_leaves context.Context.stat;
  Stat.incr_written_leaf_sizes context.Context.stat len;
  _Leaf (v, Indexed i, Hashed h), i, nh

let write_link context i index =
  check_mode context.Context.storage;
  (* |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 254 ------------------------>| *)
  let storage = context.Context.storage in
  let buf = make_buf storage i in
  C.write_string zero_24 buf 0 24;
  C.set_index buf 24 index;
  C.set_index buf 28 (Index.of_int32 (-254l));
  Stat.incr_written_links context.Context.stat

let write_internal context nl nr nh =
  check_mode context.Context.storage;
  (* internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| *)
  let storage = context.Context.storage in
  let h = Node_hash.prefix nh in

  let hstr = Hash.to_string h in
  let il = index nl in
  let ir = index nr in

  let i = Storage.new_index storage in

  let must_refer_to, i =
    if i = Index.succ il then
      (* the following index refers to the right *)
      `Right, i
    else if i = Index.succ ir then `Left, i
    else begin
      (* Fat internal *)
      (* Write the link to the right at i *)
      write_link context i ir;
      let i = Storage.new_index storage in
      `Left, i
    end 
  in

  let buf = make_buf storage i in

  (* 0 to 215 bits *)
  C.blit_from_string hstr 0 buf 0 27;

  (* fix for the 223rd and 224th bits (pos 222, 223) *)
  C.set_char buf 27
    (let c = Char.code @@ String.unsafe_get hstr 27 in
     let c = c land 0xfc in
     Char.chr (if must_refer_to = `Left then c else c lor 2));

  (* next 32bits *)
  C.set_index buf 28 (if must_refer_to = `Left then il else ir);

  Stat.incr_written_internals context.Context.stat;
  _Internal (nl, nr, Indexed i, Hashed h), i, nh

let write_empty_bud context =
  check_mode context.Context.storage;
  (* XXX No point to store the empty bud more than once... *)
  (* empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 256 ------------------------>| *)
  let storage = context.Context.storage in
  let i = Storage.new_index storage in
  let buf = make_buf storage i in
  C.write_string bud_first_28 buf 0 28;
  C.set_index buf 28 (Index.of_int32 (-256l));
  Stat.incr_written_buds context.Context.stat;
  Stat.incr_written_empty_buds context.Context.stat;
  (* XXX hash is precomputable *)
  _Bud (None, Indexed i, Hashed (Node_hash.(prefix @@ of_bud None))), i, Node_hash.of_bud None

let write_bud bud_cache context n nh = 
  check_mode context.Context.storage;
  (* bud       |<- first 222 of hash -------->|1|1| |<- the index of the child -------->| *)
  let h = Node_hash.prefix nh in
  match Bud_cache.find_opt bud_cache h with
  | Some i ->
      (* XXX debug.  remove when sure *)
      let bud = View (_Bud (Some n, Not_Indexed, Not_Hashed)) in
      let bud' = View (load_node context i Not_Extender) in
      let diffs = Diff.diff context bud bud' in
      if diffs <> [] then begin
        Log.fatal "Collision: %s" (Node_hash.to_hex_string nh);
        Log.fatal "Collision: %s" (Node_hash.to_hex_string (snd (Node_hash.compute context bud')));
        List.iter (fun diff -> Log.fatal "diff: %a" Diff.pp diff) diffs;
        Log.fatal "%a" Node.pp bud;
        Log.fatal "%a" Node.pp bud';
        exit 2
      end;
      (* We do NOT write this bud but use the existing one! *)
      _Bud (Some n, Indexed i, Hashed h), i, nh
  | _ ->
      let storage = context.Context.storage in
      let i = Storage.new_index storage in
      let buf = make_buf storage i in
      C.write_string (Hash.to_string @@ Node_hash.prefix nh) buf 0 28;
      C.set_index buf 28 @@ index n;
      Stat.incr_written_buds context.Context.stat;
      Bud_cache.add bud_cache h i;
      _Bud (Some n, Indexed i, Hashed h), i, nh

let write_extender context seg n nh =
  check_mode context.Context.storage;
  (* extender  |0*1|<- segment --|..|---------->|6bits|01| |<- the index of the child ------------>| *)
  let storage = context.Context.storage in
  let h = Node_hash.prefix nh in
  let sseg = Segment.encode seg in
  let extra_cells = (String.length sseg - 27 + 31) / 32 in
  if extra_cells > 63 then assert false;
  let zeros = extra_cells * 32 + 27 - String.length sseg in
  let i = Storage.new_indices storage (extra_cells + 1) in

  let i' = Index.(+) i (Index.of_int extra_cells) in
  let buf = get_cell storage i' in
  C.set_index buf 28 @@ index n;
  C.set_char buf 27 @@ Char.chr (extra_cells lsl 2 + 01);

  let buf = get_bytes storage i ((extra_cells + 1) * 32) in
  C.write_string (String.make zeros '\x00') buf 0 zeros;
  C.write_string sseg buf zeros (String.length sseg);

  (* Stat.incr_written_buds context.Context.stat; XXX *)

  _Extender (seg, n, Indexed i', Hashed h), i', nh

let equal context n1 n2 =
  let rec aux = function
    | [] -> Ok ()
    | (n1,n2)::rest ->
        match n1, n2 with
        | Disk (i1, ew1), Disk (i2, ew2) when i1 = i2 && ew1 = ew2 -> aux rest
        | Disk _, Disk _ -> Error (n1,n2)
        | Disk (i, ew), n2 ->
            let n1 = View (load_node context i ew) in
            aux @@ (n1,n2)::rest
        | n1, Disk (i, ew) ->
            let n2 = View (load_node context i ew) in
            aux @@ (n1,n2)::rest
        | View v1, View v2 ->
            match v1, v2 with
            | Internal (n11, n12, _, _), Internal (n21, n22, _, _) ->
                aux @@ (n11,n21)::(n12,n22)::rest
            | Bud (None, _, _), Bud (None, _, _) -> aux rest
            | Bud (Some n1, _, _), Bud (Some n2, _, _) -> aux @@ (n1,n2) :: rest
            | Leaf (v1, _, _), Leaf (v2, _, _) when v1 = v2 -> aux rest
            | Extender (seg1, n1, _, _), Extender (seg2, n2, _, _) when Segment.equal seg1 seg2 ->
                aux @@ (n1,n2)::rest
            | _ -> Error (n1,n2)
  in
  aux [(n1, n2)]

let commit_node context bud_cache node =
  if not (Context.mode context = Writer) then assert false; (* XXX *) 
  let check_hash h nh = match h with
    | Hashed h -> assert (h = Node_hash.prefix nh)
    | Not_Hashed -> ()
  in
  let storage = context.Context.storage in
  (* XXX It is not tail recursive.  May crash if the depth of 
     the tree exceeds the stack limit *)
  let rec commit_aux : node -> (node * Index.t * Node_hash.t) = function
    | Disk (index, wit) ->
        (* Need to get the hash from the disk *)
        let v = load_node context index wit in
        let nh = snd @@ Node_hash.compute context (View v) in
        View v, index, nh
    | View v -> 
        let v, i, nh = commit_aux' v in
        View v, i, nh

  and commit_aux' : view -> (view * Index.t * Node_hash.t) = fun v -> 
    match v with
    (* easy case where it's already commited *)
    | Leaf (_, Indexed i, Hashed h) 
    ->
        let nh = snd @@ Node_hash.compute context (View v) in
        assert (Node_hash.prefix nh = h);
        (v, i, nh)
    | Bud (_, Indexed i, Hashed h)
    ->
        let nh = snd @@ Node_hash.compute context (View v) in
        assert (Node_hash.prefix nh = h);
        (v, i, nh)
    | Internal (_, _, Indexed i, Hashed h)
    ->
        let nh = snd @@ Node_hash.compute context (View v) in
        assert (Node_hash.prefix nh = h);
        (v, i, nh)
    | Extender (_, _, Indexed i, Hashed h) ->
        let nh = snd @@ Node_hash.compute context (View v) in
        assert (Node_hash.prefix nh = h);
        (v, i, nh)
         
    (* indexing is necessary below.  If required, the hash is also computed *)
    | Leaf (value, Not_Indexed, h) ->
        let _v, nh = Node_hash.compute context (View v) in
        check_hash h nh;
        (* if the size of the value is 1 <= size <= 32, the contents are written
           to the previous index of the leaf *)
        let len = Value.length value in

        if len <> 0 then Stat.incr_committed_leaf_sizes context.Context.stat len;

        let create_new () =
          if len <= 32 then begin
            write_small_leaf storage value;
            write_leaf context value nh
          end else if 33 <= len && len <= 64 then begin
            write_medium_leaf storage value;
            write_leaf context value nh
          end else begin
            write_large_leaf_to_plebeia storage value;
            write_leaf context value nh
          end
        in
        if len = 0 then
          (* We don't store 0 size leaves *)
          let index = zero_sized_leaf_index in
          let nh = zero_sized_leaf_hash in
          let h = zero_sized_leaf_short_hash in
          _Leaf (value, Indexed index, Hashed h), index, nh
        else if 1 <= len && len <= (Hashcons.config context.Context.hashcons).max_leaf_size (* XXX should have an API *) then begin
          (* try hashcons *)
          let hashcons = context.Context.hashcons in
          match Hashcons.find hashcons value with
          | Error e -> Error.raise e
          | Ok (Some index) ->
              let nh = Node_hash.of_leaf value (* XXX inefficient! *) in
              let h = Node_hash.prefix nh in
              _Leaf (value, Indexed index, Hashed h), index, nh
          | Ok None -> 
              let v, i, nh = create_new () in
              begin match Hashcons.add hashcons value i with
                | Ok () -> ()
                | Error e -> Error.raise e
              end;
              (v, i, nh)
        end else create_new ()

    | Bud (Some underneath, Not_Indexed, h) ->
        let (node, _, nh') = commit_aux underneath in
        let nh = Node_hash.of_bud @@ Some nh' in
        check_hash h nh;
        write_bud bud_cache context node nh

    | Bud (None, Not_Indexed, h) ->
        let nh = Node_hash.of_bud None in
        check_hash h nh;
        write_empty_bud context

    | Internal (left, right, Not_Indexed, h) ->
        let (left, _il, nhl) = commit_aux left in
        let (right, _ir, nhr) = commit_aux right in
        let nh = Node_hash.of_internal nhl nhr in
        check_hash h nh;
        write_internal context left right nh

    | Extender (segment, underneath, Not_Indexed, h)  ->
        let (underneath, _i, nh') = commit_aux underneath in
        let nh = Node_hash.of_extender segment nh' in
        check_hash h nh;
        write_extender context segment underneath nh

    | (Leaf (_, Indexed _, Not_Hashed)|Bud (None, Indexed _, Not_Hashed)|
       Bud (Some _, Indexed _, Not_Hashed)|
       Internal (_, _, Indexed _, Not_Hashed)|
       Extender (_, _, Indexed _, Not_Hashed)) -> assert false

  in 
  let (node, i, nh) =  commit_aux node in
  (* shrink the hashcons buckets *)
  Hashcons.may_shrink context.hashcons;
  node, i, Node_hash.prefix nh

let () = Context.ref_load_leaf_value := fun ctxt i ->
    match load_node ctxt i Node.Not_Extender with
    | Leaf (v, _, _) -> Some v
    | _ -> None
