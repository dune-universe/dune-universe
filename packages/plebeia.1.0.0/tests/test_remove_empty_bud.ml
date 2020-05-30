open Plebeia.Internal
open Result
open Test_utils
open Cursor
open Node

(* XXX The first half is a copy of test_full_deep_random.ml *)

module RS = Random.State

module Debug = Debug

(* We cannot compare with Dumb *)
  
let random_seg st = random_segment ~length:3 st

let random_segs st = 
  let n = RS.int st 4 + 1 in
  List.init n (fun _ -> random_seg st)
    
let random_value = 
  let cntr = ref 0 in 
  fun _ -> incr cntr; Value.of_string @@ string_of_int !cntr

let validate context n =
  default (Debug.validate_node context n) (fun e -> 
      to_file ~file:"invalid.dot" @@ Debug.dot_of_node n;
      prerr_endline "Saved the current node to invalid.dot";
      failwith e)

let string_of_segs segs =
  String.concat "/" (List.map Segment.to_string segs)

let check_root c =
  match c with
  | Cursor (Top, _, _) -> ()
  | _ -> xassert false

(* Pick an existing random segment points at Leaf or Bud *)
let choose_random_segs st c =
  let Cursor (_, n, context) = c in
  match Node.view context n with
  | Internal _ | Extender _ | Leaf _ -> assert false
  | Bud (None, _, _) -> None (* unremovable *)
  | Bud (Some n, _, _) ->
      let rec choose_random_segs rev_segs rev_seg n =
        let v = Node.view context n in
        match v with
        | Leaf _ -> List.rev (List.rev rev_seg :: rev_segs)
        | Bud (None, _, _) -> List.rev (List.rev rev_seg :: rev_segs)
        | Bud (Some n, _, _) ->
            let rev_segs = List.rev rev_seg :: rev_segs in
            if RS.int st 2 = 0 then List.rev rev_segs
            else choose_random_segs rev_segs [] n
        | Internal (n1, n2, _, _) ->
            if RS.int st 2 = 0 then 
              let rev_seg = Segment.Left :: rev_seg in
              choose_random_segs rev_segs rev_seg n1
            else
              let rev_seg = Segment.Right :: rev_seg in
              choose_random_segs rev_segs rev_seg n2
        | Extender (seg, n, _, _) ->
            let rev_seg = List.rev_append (Segment.to_sides seg) rev_seg in
            choose_random_segs rev_segs rev_seg n
      in
      Some (List.map Segment.of_sides @@ choose_random_segs [] [] n)

let do_random st sz c =
  let rev_ops = ref [] in
  let add_op o = rev_ops := o :: !rev_ops in
  let rec f c i =
    if i = sz then c
    else 
      let c = 
        let rec get_op () = match RS.int st 8 with
          | 0 -> `Insert (random_segs st, random_value st)
          | 1 -> `Upsert (random_segs st, random_value st)
          | 2 -> `Subtree (random_segs st)
          | 3 -> `Commit
          | 4 -> 
              begin match choose_random_segs st c with
                | None -> get_op ()
                | Some segs -> 
                    let segs' = random_segs st in
                    `Copy (segs, segs')
              end
          | _ -> 
              match choose_random_segs st c with
              | None -> get_op ()
              | Some segs -> `Delete segs
        in
        let op = get_op () in
        match op with
        | `Insert (segs, v) ->
            (* Format.eprintf "Insert at %s@." @@ string_of_segs segs; *)
            begin match Deep.insert c segs v with
              | Ok c ->
                  add_op op;
                  check_root c;
                  c
              | Error _ -> c
            end
        | `Upsert (segs, v) ->
            (* Format.eprintf "Upsert at %s@." @@ string_of_segs segs; *)
            begin match Deep.upsert c segs v with
              | Ok c ->
                  add_op op;
                  check_root c;
                  c
              | Error _ -> c
            end
        | `Subtree segs ->
            (* Format.eprintf "Create_subtree at %s@." @@ string_of_segs segs; *)
            begin match Deep.create_subtree ~create_subtrees:true c segs with
              | Ok c -> 
                  add_op op;
                  check_root c;
                  c
              | Error _ -> c
            end
        | `Delete segs ->
            (* Format.eprintf "Delete at %s@." @@ string_of_segs segs; *)
            begin match 
                Deep.delete c segs
              with
              | Ok c -> 
                  add_op op;
                  check_root c;
                  c
              | Error _ -> c
            end
        | `Copy (segs, segs') ->
            begin match Deep.copy ~create_subtrees:true c segs segs' with
              | Ok c -> 
                  add_op op;
                  check_root c;
                  c
              | Error _ -> c
            end
        | `Commit ->
      (*
                  let Cursor(trail, _, _) = c in
                  let _segs = Cursor.segs_of_trail trail in
                  Format.eprintf "Commit and load: %s@." @@ string_of_segs segs;
      *)
                  let Cursor(_, _, context), i, _ = Cursor_storage.commit_top_cursor (Bud_cache.empty ()) c in
                  let v = Node_storage.load_node context i Not_Extender in
                  add_op op;
                  _Cursor (_Top, View v, context)
            in
            f c (i+1)
        in
        let c = f c 0 in
        (c, List.rev !rev_ops)

module SegsS = Set.Make(struct 
    type t = Segment.t list 
    let compare segs1 segs2 = 
      compare 
        (String.concat "/" @@ List.map Segment.to_string segs1)
        (String.concat "/" @@ List.map Segment.to_string segs2)
  end) 

let get_leaves c = 
  Cursor.fold ~init:SegsS.empty c (fun acc c ->
      let _, v = Cursor.view c in
      match v with
      | Leaf (_, _, _) -> `Continue, SegsS.add (Cursor.segs_of_cursor c) acc
      | _ -> `Continue, acc) 

let get_empty_buds c =
  Cursor.fold ~init:[] c (fun acc c ->
      let c, v = Cursor.view c in
      match c, v with
      | Cursor (Top, _, _), _ -> `Continue, acc (* ignore the top *)
      | _, Bud (None, _, _) -> `Continue, Cursor.segs_of_cursor c :: acc
      | _ -> `Continue, acc) 
      
let () = 
  let st = RS.make_self_init () in
  let removed = ref 0 in
  for _i = 0 to 1000 do
    test_with_cursor @@ fun c ->
    let c, _ops = do_random st 1000 c in
    let leaves = get_leaves c in
    let empty_buds = get_empty_buds c in
    let c = List.fold_left (fun c segs ->
        let c = from_Ok @@ Deep.subtree c segs in
        match c with
        | Cursor (Top, _, _) -> c (* we cannot remove the top *)
        | _ -> 
            let c = Error.from_Ok @@ Cursor.remove_empty_bud c in
            from_Ok @@ Cursor.go_top c) c empty_buds
    in
    let leaves' = get_leaves c in
    let empty_buds' = get_empty_buds c in
    if not @@ SegsS.equal leaves leaves' then begin
      prerr_endline "leaves";
      List.iter (fun segs -> 
          prerr_endline @@ String.concat "/" (List.map Segment.to_string segs))
        (SegsS.elements leaves);
      prerr_endline "AND...";
      prerr_endline "leaves'";
      List.iter (fun segs -> 
          prerr_endline @@ String.concat "/" (List.map Segment.to_string segs))
        (SegsS.elements leaves');
    end;
    assert (SegsS.equal leaves leaves');
    assert (empty_buds' = []);
    removed := !removed + List.length empty_buds;
    
    ignore @@ List.fold_left (fun (c, old_leaves) leaf ->
        let c = Error.from_Ok @@ Deep.delete_and_clean_empty c leaf in
        let c = Error.from_Ok @@ Cursor.go_top c in
        let empty_buds = get_empty_buds c in
        let new_leaves = get_leaves c in
        assert (empty_buds = []);

        if not @@ SegsS.equal old_leaves (SegsS.add leaf new_leaves) then begin
          prerr_endline "leaves";
          List.iter (fun segs -> 
              prerr_endline @@ String.concat "/" (List.map Segment.to_string segs))
            (SegsS.elements old_leaves);
          prerr_endline "AND...";
          prerr_endline "leaves'";
          List.iter (fun segs -> 
              prerr_endline @@ String.concat "/" (List.map Segment.to_string segs))
            (SegsS.elements new_leaves);
        end;

        Format.eprintf "#new_leaves = %d@." (List.length (SegsS.elements new_leaves));

        assert (SegsS.equal (SegsS.add leaf new_leaves) old_leaves);
        (c, new_leaves)) (c, leaves) @@ shuffle st @@ SegsS.elements leaves
  done;
  Format.eprintf "Removed %d empty buds@." !removed

