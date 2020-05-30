open Plebeia.Internal
open Test_utils
open Cursor

let () = test_with_cursor @@ fun c ->
  let c = ok_or_fail @@ create_subtree c (path "LLL") in
  let c = ok_or_fail @@ create_subtree c (path "LLRR") in
  let c, _, _ = Cursor_storage.commit_top_cursor (Bud_cache.empty ()) c in
  let c = 
    match access_gen c (path "LLR") with
    | Ok (Reached (c, _v)) -> 
       from_Some @@ may_forget c
    | _ -> assert false
  in
  let c = ok_or_fail @@ go_top c in
  to_file ~file:"remove_up_disk.dot" @@ Debug.dot_of_cursor c;
  match delete c (path "LLL") with
  | Ok c -> 
     prerr_endline "ok";
     to_file ~file:"remove_up_disk2.dot" @@ Debug.dot_of_cursor c
  | Error _ -> prerr_endline "error"
