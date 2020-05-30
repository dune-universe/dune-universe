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
(* Implementation of the root table.
   
   All the data should be in memory.
*)

open Utils.Open

module C = Xcstruct

let zero_then_none x = if x = Index.zero then None else Some x

module RootHash : sig
  type t
    
  val of_string : string -> t
  val of_plebeia_hash : Hash.t -> t
  val to_string : t -> string
  val to_hex_string : t -> string
end = struct
  
  type t = string

  let of_string s = 
    assert (String.length s = 32);
    s
    
  let to_string x = x

  let of_plebeia_hash h =  
    let s = Hash.to_string h in
    s ^ "\000\000\000\000"
    
  let to_hex_string s = 
    let `Hex h = Hex.of_string s in
    h
end
  
module Entry = struct
  type t = 
    { meta   : string (* Log message 20 bytes *)
    ; prev   : Index.t option (* Previous entry index *)
    ; parent : Index.t option (* Index of the parent root node *)
    ; index  : Index.t (* Index of the root node *)
    ; hash   : RootHash.t (* Context hash *)
    }
  
  let pp ppf { index ; parent ; meta=_ ; hash ; _ } =
    let f fmt = Format.fprintf ppf fmt in
    f "%s at %Ld (parent=%a)" 
      (RootHash.to_hex_string hash) 
      (Index.to_int64 index)
      (fun _ppf -> function
         | None -> f "none"
         | Some x -> f "%Ld" (Index.to_int64 x)) parent

  type tables = (* all are in the memory *)
    { tbl        : (RootHash.t, t) Hashtbl.t   
    ; by_index   : (Index.t, t) Hashtbl.t
    ; children   : (Index.t, t list) Hashtbl.t
    ; mutable the_latest : t option
    }

  let empty_tables () = 
    { tbl = Hashtbl.create 101
    ; by_index = Hashtbl.create 101
    ; children = Hashtbl.create 101
    ; the_latest = None
    }

  let add tables ent =
    (* collisions are overridden *)
    Hashtbl.replace tables.tbl ent.hash ent;
    Hashtbl.replace tables.by_index ent.index ent;
    tables.the_latest <- Some ent;
    match ent.parent with
    | None -> ()
    | Some i ->
        let entries = 
          match Hashtbl.find_opt tables.children i with
          | None -> []
          | Some entries -> entries
        in
        Hashtbl.replace tables.children i (ent::entries)

  let genesis t =
    Hashtbl.fold (fun _hash entry acc ->
        if entry.parent = None then entry::acc
        else acc) t.tbl []

  let children t e =
    match Hashtbl.find_opt t.children e.index with
    | None -> []
    | Some xs -> xs

  let fold f roots init = 
    let rec loop st ent = 
      let st = ent::st in
      match ent.prev with
      | None -> st
      | Some i -> 
          match Hashtbl.find_opt roots.by_index i with
          | None -> assert false (* the table broken! *)
          | Some ent -> loop st ent
    in
    match roots.the_latest with
    | None -> init
    | Some ent ->
        List.fold_left (fun acc ent -> f ent acc) init @@ loop [] ent

  let fold_breadth_first f roots init =
    let rec loop acc = function
      | [] -> acc
      | e::es ->
          let acc = f e acc in
          let es' = children roots e in
          loop acc (es @ es')
    in
    loop init @@ genesis roots

  let iter f roots = Hashtbl.iter (fun _ e -> f e) roots.tbl

  let length roots = Hashtbl.length roots.tbl

  let to_seq roots = Seq.map snd @@ Hashtbl.to_seq roots.tbl
end

type entry = Entry.t =
  { meta   : string (* Log message 20 bytes *)
  ; prev   : Index.t option (* Previous entry index *)
  ; parent : Index.t option (* Index of the parent root node *)
  ; index  : Index.t (* Index of the root node *)
  ; hash   : RootHash.t (* Context hash *)
  }


module Commit = struct
  (* commits
  
  |0        19|20      23|24        27|28      31|
  |<- meta  ->|<- prev ->|<- parent ->|<- idx  ->|
  
  Previous cell:
  |0                                           31|
  |<-------------------- hash ------------------>|
  
  Hash value can be different from Plebeia's root Merkle hash
  
  *)
  
  let read storage i =
    let buf = Storage.make_buf storage i in
    let index = C.get_index buf 28 in
    let parent = zero_then_none @@ C.get_index buf 24 in
    let prev = zero_then_none @@ C.get_index buf 20 in
    let meta = C.copy buf 0 20 in
    let i = Index.pred i in
    let buf = Storage.make_buf storage i in
    let hash = RootHash.of_string @@ C.copy buf 0 32 in
    { index ; parent ; prev ; meta ; hash }

  let write storage commit =
    let i = Storage.new_indices storage 2 in

    (* debug *)
    assert (Option.default Index.zero commit.prev < i);

    let buf = Storage.make_buf storage i in
    C.write_string (RootHash.to_string commit.hash) buf 0 32;
    let i = Index.succ i in
    let buf = Storage.make_buf storage i in
    C.set_index buf 28 commit.index;
    C.set_index buf 24 (Option.default Index.zero commit.parent);
    C.set_index buf 20 (Option.default Index.zero commit.prev);
    C.write_string commit.meta buf 0 20;
    
    (* debug *)
    assert (commit = read storage i);
    
    i
end


(* storage *)

type t = 
  { tables : Entry.tables
  ; storage_context : Storage.t
  ; storage_roots : Storage.t (* separate file to store roots *)
  }

let read_commits storage =
  let tables = Entry.empty_tables () in
  let cntr = ref 0 in
  let rec aux = function
    | None -> ()
    | Some i ->
        let commit = Commit.read storage i in
        let ent = commit in
        if Hashtbl.mem tables.tbl commit.hash then begin
          (* A commit with the same commit_hash exists. *)
          (* We do not terminate the program but uses 
             the newer commit which has been read already.
          *)
          Log.notice "Roots.read_commits: commits with the same hash: %s at indices %d and %d.  Use the one at %d"
            (RootHash.to_hex_string  commit.hash)
            (Index.to_int i)
            (Index.to_int commit.index)
            (Index.to_int commit.index);
        end else begin
          Entry.add tables ent;
        end;
        incr cntr;
        if !cntr mod 10000 = 0 then begin
          Log.notice "read %d commits" !cntr;
        end;
        aux commit.prev
  in
  aux (Storage.get_last_root_index storage);
  Log.notice "read %d commits (%d unique) from %s" 
    !cntr (Hashtbl.length tables.tbl)
    (Storage.filename storage);
  tables

(* This is always read from storage_context *)
let read_additional_commits tables storage =
  let cntr = ref 0 in
  let rec aux = function
    | None -> Ok !cntr
    | Some i (* This i is for the root, not the commit *) ->
        let commit = Commit.read storage i in
        let entry = commit in
        match Hashtbl.find_opt tables.Entry.by_index entry.Entry.index with
        | Some entry' ->
            (* entries contain different prev fields, so they must be ignored *)
            if { entry with prev= None } = { entry' with prev= None }then 
              (* Ok, we catch up *)
              Ok !cntr
            else
              (* Oops, conflict found *)
              (* XXX nice message *)
              Error ()
        | None ->
            if Hashtbl.mem tables.tbl entry.hash then begin
              (* A commit with the same commit_hash exists. *)
              (* We do not terminate the program but uses 
                 the newer commit which has been read already.
              *)
              Log.notice "Roots.read_commits: commits with the same hash: %s at indices %d and %d.  Use the one at %d"
                (RootHash.to_hex_string  entry.hash)
                (Index.to_int i)
                (Index.to_int commit.index)
                (Index.to_int commit.index);
            end else begin
              Entry.add tables entry;
            end;
            incr cntr;
            if !cntr mod 10000 = 0 then begin
              Log.notice "read %d new commits" !cntr;
            end;
            aux commit.prev
  in
  match aux (Storage.get_last_root_index storage) with
  | Ok n -> 
      Log.notice "read %d new commits from %s" 
        !cntr
        (Storage.filename storage);
      Ok n
  | Error () -> Error ()

let write_commit storage ?parent index ~meta ~hash =
  let prev = Storage.get_last_root_index storage in
  let commit = { prev ; index ; parent ; meta ; hash } in
  let i = Commit.write storage commit in
  Storage.set_last_root_index storage (Some i);
  Storage.commit storage;
  commit

(* hash collisions are overridden *)
let add t ?parent hash index ~meta =
  assert (String.length meta = 20);
  let commit = write_commit t.storage_context ?parent index ~meta ~hash in
  Entry.add t.tables commit;
  (* Small risk of program crash before it writes to the storage_roots *)
  let _commit_in_roots = write_commit t.storage_roots ?parent index ~meta ~hash in
  Log.notice "Added root %a" Entry.pp commit

let mem t = Hashtbl.mem t.tables.tbl

let find t = Hashtbl.find_opt t.tables.tbl

let find_by_index t = Hashtbl.find_opt t.tables.by_index

let create ~storage_context ~storage_roots = 
  (* Storage_roots:  a backup for fast loading.  If anything bad happens
     in it, we load everything from storage_context.

     Storage_context: the master data which is slow to load.
  *)

  let tables, init_roots = 
    match read_commits storage_roots with
    | exception _ ->
        Log.warning "Roots file has a problem. Reloading all the roots from context file";
        read_commits storage_context, true
    | tables ->
        (* Maybe the storage_roots is obsolete or conflicting. *)
        match read_additional_commits tables storage_context with
        | Error _ ->
            Log.warning "Roots file has a conflict. Reloading all the roots from context file";
            read_commits storage_context, true
        | Ok 0 -> tables, false
        | Ok _ -> tables, true
  in
  if init_roots && Storage.mode storage_roots = Storage.Writer then begin
    Log.notice "Reinitializing roots file";
    let (), secs = with_time @@ fun () ->
        Storage.truncate storage_roots ;
        let f { index ; parent ; meta ; hash ; _ } () =
          ignore @@ write_commit storage_roots ?parent index ~meta ~hash;
        in
        Entry.fold f tables ()
    in
    Log.notice "Reinitialized roots file in %.1f secs" secs
  end;
  { tables
  ; storage_context
  ; storage_roots
  }

let read_additional_commits t = 
  read_additional_commits t.tables t.storage_context
  
let sync t =
  Storage.sync t.storage_context;
  Storage.sync t.storage_roots;
  match read_additional_commits t with
  | Ok n -> Format.eprintf "%d new commits found@." n
  | Error () -> assert false (* XXX error, unrecoverable *)

let genesis t = Entry.genesis t.tables
let children t = Entry.children t.tables
let fold f roots = Entry.fold f roots.tables
let fold_breadth_first f roots init = Entry.fold_breadth_first f roots.tables init
let iter f roots = Entry.iter f roots.tables
let length roots = Entry.length roots.tables
let to_seq roots = Entry.to_seq roots.tables
let get_latest roots = roots.tables.Entry.the_latest
