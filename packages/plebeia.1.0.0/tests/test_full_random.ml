open Plebeia.Internal
open Result
open Test_utils
open Cursor
open Node

module RS = Random.State

module Debug = Debug

module Dumb = Dumb

let random_seg st = random_segment ~length:(RS.int st 5 + 3) st
    
let random_value st = Value.of_string @@ string_of_int @@ RS.int st 30

let validate context n =
  default (Debug.validate_node context n) (fun e -> 
      to_file ~file:"invalid.dot" @@ Debug.dot_of_node n;
      prerr_endline "Saved the current node to invalid.dot";
      failwith e)

let compare_trees dumb (Cursor (_, n, context) as c) =
  (* compare the entire tree *)
  if Dumb.get_node dumb <> Dumb.of_plebeia_node context n then begin
    to_file ~file:"dumb.dot" @@ Dumb.dot_of_cursor dumb;
    to_file ~file:"plebeia.dot" @@ Debug.dot_of_cursor c;
    to_file ~file:"plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
    begin match go_top c with
      | Error _ -> prerr_endline "no root dump"
      | Ok (Cursor (_, n, context)) ->
          to_file ~file:"plebeia_dumb_root.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n;
    end;
    assert false
  end

let do_random st sz c dumb =
  let module Segs = Set.Make( struct type t = Segment.t let compare = compare end ) in
  let segs = ref Segs.empty in
  let add_seg seg = segs := Segs.add seg !segs in
  let del_seg seg = segs := Segs.remove seg !segs in
  let pick_seg st =
    let segs = Segs.elements !segs in
    let len = List.length segs in
    if len < 20 then random_seg st
    else 
      let pos = RS.int st (List.length segs) in
      List.nth segs pos
  in
  let rev_ops = ref [] in
  let add_op o = rev_ops := o :: !rev_ops in
  let rec f c dumb i =
    if i = sz then (c, dumb)
    else 
      let (c, dumb) = 
        let op = match RS.int st 8 with
          | 0 -> `Insert (random_seg st, random_value st)
          | 1 -> `Upsert (random_seg st, random_value st)
          | 2 -> `Subtree (random_seg st)
          | 3 -> `Commit
          | _ -> `Delete (pick_seg st)
        in
        match op with
        | `Insert (seg, v) ->
            begin match 
              insert c seg v,
              Dumb.insert dumb seg v
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                (* check the invariants of the node *)
                validate context n;
                add_op op;
                add_seg seg;
                (c, dumb)
            | Error _, Error _ -> 
                (c, dumb)
            | _ -> assert false
          end

        | `Upsert (seg, v) ->
            begin match 
              upsert c seg v,
              Dumb.upsert dumb seg v
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                (* check the invariants of the node *)
                validate context n;
                add_op op;
                add_seg seg;
                (c, dumb)
            | Error _, Error _ -> 
                (c, dumb)
            | _ -> assert false
          end

        | `Subtree seg ->
            begin match 
                create_subtree c seg,
                Dumb.create_subtree dumb seg
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                (* check the invariants of the node *)
                validate context n;
                add_op op;
                add_seg seg;
                (c, dumb)
            | Error _, Error _ -> 
                (c, dumb)
            | _ -> assert false
          end

        | `Delete seg ->
            begin match 
                delete c seg,
                Dumb.delete dumb seg
            with
            | Ok c, Ok dumb ->
                compare_trees dumb c;
                let Cursor (_, n, context) = c in
                (* check the invariants of the node *)
                validate context n;
                add_op op;
                del_seg seg;
                (c, dumb)
            | Error _, Error _ -> 
                (c, dumb)
            | _ -> assert false
          end

        | `Commit ->
            let Cursor(_, _, context), i, _ = Cursor_storage.commit_top_cursor (Bud_cache.empty ()) c in
            let v = Node_storage.load_node context i Not_Extender in
            add_op op;
            (_Cursor (_Top, View v, context), dumb)
      in
      f c dumb (i+1)
  in
  let (c,_) = f c dumb 0 in
  (List.rev !rev_ops, Cursor_storage.load_fully c)

let () = 
  let st = RS.make_self_init () in
  for i = 0 to 1000 do
    test_with_cursor @@ fun c ->
    let (ops, _c) = do_random st 1000 c (Dumb.empty ()) in
    (* Format.eprintf "%d ops@." (List.length ops); *)
    if i mod 100 = 0 then begin
      Format.eprintf "%d done (%d ops)@." i
        (List.length ops);
(*
      Debug.save_cursor_to_dot (Printf.sprintf "random%d.dot" i) c
*)
    end
  done
