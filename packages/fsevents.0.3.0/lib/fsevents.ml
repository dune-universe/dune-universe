(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

module T = Types.C(Types_detected)
module C = Bindings.C(Generated)

module CreateFlags = C.CreateFlags

module EventFlags = struct
  include C.EventFlags

  let string_of_must_scan_subdirs = function
    | Some { user = true; kernel = true } -> "user+kernel"
    | Some { user = true; _ } -> "user"
    | Some { kernel = true; _ } -> "kernel"
    | Some { user = false; kernel = false } -> "unknown"
    | None -> "false"

  let string_of_item_type = function
    | None -> "false"
    | Some File -> "file"
    | Some Dir -> "dir"
    | Some Symlink -> "symlink"
    | Some Hardlink -> "hardlink"

  let to_string t =
    Printf.sprintf
      "{\n  must_scan_subdirs     = %s;\
        \n  event_ids_wrapped     = %b;\
        \n  history_done          = %b;\
        \n  root_changed          = %b;\
        \n  mount                 = %b;\
        \n  unmount               = %b;\
        \n  own_event             = %b;\
        \n  item_created          = %b;\
        \n  item_removed          = %b;\
        \n  item_inode_meta_mod   = %b;\
        \n  item_renamed          = %b;\
        \n  item_modified         = %b;\
        \n  item_finder_info_mod  = %b;\
        \n  item_change_owner     = %b;\
        \n  item_xattr_mod        = %b;\
        \n  item_type             = %s;\
        \n  item_is_last_hardlink = %b;\
        \n}"
      (string_of_must_scan_subdirs t.must_scan_subdirs)
      t.event_ids_wrapped
      t.history_done
      t.root_changed
      t.mount
      t.unmount
      t.own_event
      t.item_created
      t.item_removed
      t.item_inode_meta_mod
      t.item_renamed
      t.item_modified
      t.item_finder_info_mod
      t.item_change_owner
      t.item_xattr_mod
      (string_of_item_type t.item_type)
      t.item_is_last_hardlink

  let to_string_one_line t =
    Printf.sprintf "%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s"
      (match t.must_scan_subdirs with
       | None -> ""
       | Some v ->
         "MustScanSubdirs("^(string_of_must_scan_subdirs (Some v))^") "
      )
      (if t.event_ids_wrapped then "EventIdsWrapped " else "")
      (if t.history_done then "HistoryDone " else "")
      (if t.root_changed then "RootChanged " else "")
      (if t.mount then "Mount " else "")
      (if t.unmount then "Unmount " else "")
      (if t.own_event then "OwnEvent " else "")
      (if t.item_created then "ItemCreated " else "")
      (if t.item_removed then "ItemRemoved " else "")
      (if t.item_inode_meta_mod then "ItemInodeMetaMod " else "")
      (if t.item_renamed then "ItemRenamed " else "")
      (if t.item_modified then "ItemModified " else "")
      (if t.item_finder_info_mod then "ItemFinderInfoMod " else "")
      (if t.item_change_owner then "ItemChangeOwner " else "")
      (if t.item_xattr_mod then "ItemXattrMod " else "")
      (match t.item_type with
       | None -> ""
       | Some v -> "ItemType("^(string_of_item_type (Some v))^") "
      )
      (if t.item_is_last_hardlink then "ItemIsLastHardlink " else "")

  let zero = {
    must_scan_subdirs = None;
    event_ids_wrapped = false;
    history_done = false;
    root_changed = false;
    mount = false;
    unmount = false;
    own_event = false;
    item_created = false;
    item_removed = false;
    item_inode_meta_mod = false;
    item_renamed = false;
    item_modified = false;
    item_finder_info_mod = false;
    item_change_owner = false;
    item_xattr_mod = false;
    item_type = None;
    item_is_last_hardlink = false;
  }
end

module EventId = struct
  include C.EventId

  let min a b = match a, b with
    | Now, _ -> b
    | _, Now -> a
    | Since x, Since y -> match Unsigned.UInt64.compare x y with
      | x when x > 0 -> b
      | _ -> a

  let max a b = match a, b with
    | Now, _ -> a
    | _, Now -> b
    | Since x, Since y -> match Unsigned.UInt64.compare x y with
      | x when x < 0 -> b
      | _ -> a

  let compare a b = match a, b with
    | Now, Now -> 0
    | Now, Since _ -> 1
    | Since _, Now -> -1
    | Since a, Since b -> Unsigned.UInt64.compare a b
end

type t = {
  stream : C.t;
  callback :
    C.t ->
    unit Ctypes_static.ptr ->
    Unsigned.size_t ->
    string Ctypes.ptr ->
    C.EventFlags.t Ctypes.ptr ->
    Unsigned.uint64 Ctypes.ptr -> unit
}

type callback = C.Callback.t

let create ?(since=EventId.Now) latency flags f paths =
  let callback = C.Callback.to_cstring_typ f in
  let stream = C.create None callback None paths since latency flags in
  { stream; callback }

let get_latest_event_id { stream; _ } = C.get_latest_event_id stream

let schedule_with_run_loop { stream; _ } = C.schedule_with_run_loop stream

let start { stream; _ } = C.start stream

let flush_sync { stream; _ } = C.flush_sync stream

let stop { stream; _ } = C.stop stream

let invalidate { stream; _ } = C.invalidate stream

let release { stream; _ } = C.release stream

let copy_paths_being_watched { stream; _ } = C.copy_paths_being_watched stream

module Types = Types
module Types_detected = Types_detected
module Bindings = Bindings
module Generated = Generated
