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
open Utils.Open
open Node
open Cursor

type t =
  { roots : Roots.t
  ; context : Context.t
  }

let roots { roots ; _ } = roots
let context { context ; _ } = context

let empty_cursor t = Cursor.empty t.context

let check_prefix s =
  if s = "" then failwith "Path prefix cannot be empty"
  else if s.[String.length s - 1] = '/' then failwith "Path prefix cannot be a directory"
  else ()

let create ?context_pos ?context_length ?hashcons path_prefix =
  check_prefix path_prefix;
  let _lock = Lock.lock @@ path_prefix ^ ".lock" in
  let context =
    Context.create ?pos:context_pos ?length:context_length
      ?hashcons
      (path_prefix ^ ".context")
  in
  let roots = 
    Roots.create 
      ~storage_context: context.Context.storage 
      ~storage_roots: (Storage.create @@ path_prefix ^ ".roots")
  in
  { roots ; context }

let open_ ~mode ?context_pos ?hashcons path_prefix =
  check_prefix path_prefix;
  if mode = Storage.Writer then ignore @@ Lock.lock @@ path_prefix ^ ".lock";
  let context =
    Context.open_ ?pos:context_pos ~mode 
      ?hashcons
      (path_prefix ^ ".context")
  in
  let roots = 
    Roots.create 
      ~storage_context: context.Context.storage 
      ~storage_roots: (Storage.open_ ~mode (path_prefix ^ ".roots"))
  in
  { roots ; context }

let close { context ; _ } = Context.close context

let commit 
    ?(allow_missing_parent=false) 
    ?(override=false) 
    bud_cache 
    { roots ; context ; _ }
    ~parent 
    ~meta 
    ?hash_override
    c =
  (* make sure contexts are the same *)
  let () =
    let Cursor (_, _, context') = c in
    assert (context == context'); (* XXX should not use pointer equality *)
  in

  let (c, i, h) = Cursor_storage.commit_top_cursor bud_cache c in
  let hash = match hash_override with
    | None -> Roots.RootHash.of_plebeia_hash h
    | Some hash -> hash
  in
  Log.notice "Vc.commit hash:%s  plebeia_hash:%s  parent:%s"
    (Roots.RootHash.to_hex_string hash)
    (Hash.to_hex_string h)
    (match parent with None -> "none" | Some h -> Roots.RootHash.to_hex_string h);

  begin match Roots.find roots hash with
    | Some _i' ->
        if override then
          Log.notice "Vc: Overriding hash %s" (Roots.RootHash.to_hex_string hash)
        else
          failwithf "hash collision %s" (Hash.to_hex_string h)
    | None -> ()
  end;

  let parent = match parent with
    | None -> None
    | Some h ->
        match Roots.find roots h with
        | None when not allow_missing_parent -> failwithf "No such parent: %s@." (Roots.RootHash.to_hex_string h)
        | None -> None
        | Some { Roots.index ; _ } -> Some index
  in
  Roots.add roots ?parent hash i ~meta;
  Storage.commit context.Context.storage;
  (c, h)

let sync t =
  Log.notice "Vc: sync'ing storage and roots";
  let (), secs = with_time (fun () -> Roots.sync t.roots) in
  Log.notice "Vc: reloaded storage and roots in %.1f secs" secs

let checkout ({ roots ; context ; _ } as t) hash =
  match Roots.find roots hash with
  | Some { Roots.index ; _ } ->
      Some (_Cursor (_Top,
                     Disk (index, Not_Extender),
                     context))
  | None when Context.mode context = Writer -> 
      (* Writer knows everything.  hash really does not exist. *)
      None 
  | None -> 
      (* XXX if it is a reader, it does not know the new roots written by
         the writer.  The reader should update the roots and retry 
         the checkout *)
      sync t;
      match Roots.find roots hash with
      | Some { Roots.index ; _ } ->
          Some (_Cursor (_Top, 
                         Disk (index, Not_Extender),
                         context))
      | None -> None (* Really not found *)
