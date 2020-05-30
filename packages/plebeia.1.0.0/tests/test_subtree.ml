(*

   Random insert/upsert/rm/mkdir/rmdir/commit test.
   
*)
open Plebeia.Internal
open Result
open Test_utils
open Cursor

module RS = Random.State

let dump_cursor c =
  let Cursor (_, n, context) = c in
  to_file ~file:"plebeia.dot" @@ Debug.dot_of_cursor c;
  to_file ~file:"plebeia_dumb.dot" @@ Dumb.dot_of_node @@ Dumb.of_plebeia_node context n
  
let validate context n =
  default (Debug.validate_node context n) (fun e -> 
      to_file ~file:"invalid.dot" @@ Debug.dot_of_node n;
      prerr_endline "Saved the current node to invalid.dot";
      failwith e)

let random_segments st = 
  let len = RS.int st 3 + 1 in
  List.init len (fun _ -> random_segment ~length:3 st)

let doit st (rev_hist, stat_subdirs, stat_inserts, stat_deletes, stat_commits) cur =
  let add_hist x = rev_hist := x :: !rev_hist in
  let random_value st = Value.of_string @@ string_of_int @@ RS.int st 10 in
  let subs = random_segments st in
  let seg = random_segment ~length:3 st in
  let rec f cur = function
    | sub::subs ->
        begin match ok_or_fail @@ access_gen cur sub with
          | Reached (_, Bud _) ->
              add_hist @@ `subtree sub;
              let cur = ok_or_fail @@ subtree cur sub in
              f cur subs
          | Middle_of_extender (_, _, _, x) when Segment.is_empty x -> cur (* skip *)
          | Empty_bud | Middle_of_extender (_, _, _, _) ->
              add_hist @@ `create_subtree sub;
              let cur = ok_or_fail @@ create_subtree cur sub in
              incr stat_subdirs;
              ok_or_fail @@ subtree cur sub
          | _ -> cur (* skip *)
        end
    | [] -> 
        match ok_or_fail @@ access_gen cur seg with
        | Reached (_, Leaf _) ->
            begin match get cur seg with
            | Ok _ -> 
                begin match RS.int st 2 with
                  | 0 -> 
                      add_hist @@ `upsert seg;
                      ok_or_fail @@ upsert cur seg @@ random_value st
                  | 1 -> 
                      incr stat_deletes;
                      add_hist @@ `delete seg;
                      ok_or_fail @@ delete cur seg
                  | _ -> assert false
                end
            | Error _ -> assert false
            end
        | Reached (_, Bud _) ->
            add_hist @@ `delete seg;
            ok_or_fail @@ delete cur seg
        | Middle_of_extender (_, _, _, x) when Segment.is_empty x -> cur (* skip *)
        | Empty_bud | Middle_of_extender (_, _, _, _) ->
            incr stat_inserts;
            add_hist @@ `insert seg;
            ok_or_fail @@ insert cur seg @@ random_value st
        | _ -> cur (* skip *)
  in
  let cur = f cur subs in
  add_hist @@ `go_top;
  let cur = ok_or_fail @@ go_top cur in
  if RS.int st 10 = 0 then 
    let (cur, _, _) = 
      incr stat_commits; 
      add_hist `commit;
      Cursor_storage.commit_top_cursor (Bud_cache.empty ()) cur 
    in cur
  else cur

let print_hist = function
  | `commit -> Format.eprintf "  let c, _, _ = Cursor_storage.commit_cursor c in@."
  | `subtree s -> Format.eprintf "  let c = ok_or_fail @@@@ subtree c (path \"%s\") in@." (Segment.to_string s)
  | `create_subtree s -> Format.eprintf "  let c = ok_or_fail @@@@ create_subtree c (path \"%s\") in@." (Segment.to_string s)
  | `insert s -> Format.eprintf "  let c = ok_or_fail @@@@ insert c (path \"%s\") (value \"1\") in@." (Segment.to_string s)
  | `upsert s -> Format.eprintf "  let c = ok_or_fail @@@@ upsert c (path \"%s\") (value \"1\") in@." (Segment.to_string s)
  | `delete s -> Format.eprintf "  let c = ok_or_fail @@@@ delete c (path \"%s\") in@." (Segment.to_string s)
  | `go_top -> Format.eprintf "  let c = ok_or_fail @@@@ go_top c in@."
  
let test st size =
  let rev_hist = ref [] in
  let stat_subdirs = ref 0 in
  let stat_inserts = ref 0 in
  let stat_deletes = ref 0 in
  let stat_commits = ref 0 in
  let rec f i cur =
    if i = size then ()
    else
      let cur = doit st (rev_hist, stat_subdirs, stat_inserts, stat_deletes, stat_commits) cur in
      f (i+1) cur
  in
  test_with_cursor @@ fun cur -> 
  let res = try Ok (f 0 cur) with e -> Error e in
  Format.eprintf "subdirs=%d inserts=%d deletes=%d commits=%d@."
    !stat_subdirs !stat_inserts !stat_deletes !stat_commits;
  match res with
  | Ok () -> Ok ()
  | Error e -> Error (e, List.rev !rev_hist)
    
let () = 
  let st = RS.make_self_init () in
  let rec f acc = function
    | 0 -> if acc then assert false else ()
    | n ->
        match test st 1000 with
        | Ok () -> f acc (n - 1)
        | Error (e, hist) ->
            (* if fails, we try until finding a short example *)
            let len = List.length hist in
            if len < 50 then begin
              List.iter print_hist hist;
              raise e
            end else begin
              Format.eprintf "Error ops=%d@." len;
              f true (n - 1)
            end
  in
  f false 1000

