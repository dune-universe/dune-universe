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

open Ctypes

let (|||) = Int32.logor

let (??>) flag int32 = if flag then int32 else 0_l

let (??<) field int32 = Int32.logand field int32 <> 0_l

module T = Types.C(Types_detected)

module C(F: Cstubs.FOREIGN) = struct

  type t = unit ptr

  (* typedef struct __FSEventStream* FSEventStreamRef; *)
  let typ : t typ = typedef (ptr void) "FSEventStreamRef"

  let const_typ : t typ = typedef typ "ConstFSEventStreamRef"

  module CreateFlags = struct

    open T.CreateFlags

    type t = {
      use_cf_types : bool;
      no_defer : bool;
      watch_root : bool;
      ignore_self : bool;
      file_events : bool;
      mark_self : bool;
    }

    let empty = {
      use_cf_types = false;
      no_defer = false;
      watch_root = false;
      ignore_self = false;
      file_events = false;
      mark_self = false;
    }

    let detailed_interactive = {
      use_cf_types = false;
      no_defer = true;
      watch_root = true;
      ignore_self = false;
      file_events = true;
      mark_self = false;
    }

    let use_cf_types_i = Unsigned.UInt32.to_int32 use_cf_types
    let no_defer_i     = Unsigned.UInt32.to_int32 no_defer
    let watch_root_i   = Unsigned.UInt32.to_int32 watch_root
    let ignore_self_i  = Unsigned.UInt32.to_int32 ignore_self
    let file_events_i  = Unsigned.UInt32.to_int32 file_events
    let mark_self_i    = Unsigned.UInt32.to_int32 mark_self

    let to_uint32 {
      use_cf_types;
      no_defer;
      watch_root;
      ignore_self;
      file_events;
      mark_self;
    } = Unsigned.UInt32.of_int32 (
      (??> use_cf_types use_cf_types_i) |||
      (??> no_defer     no_defer_i) |||
      (??> watch_root   watch_root_i) |||
      (??> ignore_self  ignore_self_i) |||
      (??> file_events  file_events_i) |||
      (??> mark_self    mark_self_i)
    )

    let of_uint32 i =
      let i = Unsigned.UInt32.to_int32 i in
      {
        use_cf_types = ??< i use_cf_types_i;
        no_defer     = ??< i no_defer_i;
        watch_root   = ??< i watch_root_i;
        ignore_self  = ??< i ignore_self_i;
        file_events  = ??< i file_events_i;
        mark_self    = ??< i mark_self_i;
      }

    let typ = view ~read:of_uint32 ~write:to_uint32 t

  end

  module EventFlags = struct

    open T.EventFlags

    type dropping_party = {
      user : bool;
      kernel: bool;
    }

    type item_type = File | Symlink | Dir | Hardlink

    type t = {
      must_scan_subdirs    : dropping_party option;
      event_ids_wrapped    : bool;
      history_done         : bool;
      root_changed         : bool;
      mount                : bool;
      unmount              : bool;
      own_event            : bool;
      item_created         : bool;
      item_removed         : bool;
      item_inode_meta_mod  : bool;
      item_renamed         : bool;
      item_modified        : bool;
      item_finder_info_mod : bool;
      item_change_owner    : bool;
      item_xattr_mod       : bool;
      item_type            : item_type option;
      item_is_last_hardlink: bool;
    }

    let must_scan_subdirs_i    = Unsigned.UInt32.to_int32 must_scan_subdirs
    let user_dropped_i         = Unsigned.UInt32.to_int32 user_dropped
    let kernel_dropped_i       = Unsigned.UInt32.to_int32 kernel_dropped
    let event_ids_wrapped_i    = Unsigned.UInt32.to_int32 event_ids_wrapped
    let history_done_i         = Unsigned.UInt32.to_int32 history_done
    let root_changed_i         = Unsigned.UInt32.to_int32 root_changed
    let mount_i                = Unsigned.UInt32.to_int32 mount
    let unmount_i              = Unsigned.UInt32.to_int32 unmount
    let own_event_i            = Unsigned.UInt32.to_int32 own_event
    let item_created_i         = Unsigned.UInt32.to_int32 item_created
    let item_removed_i         = Unsigned.UInt32.to_int32 item_removed
    let item_inode_meta_mod_i  = Unsigned.UInt32.to_int32 item_inode_meta_mod
    let item_renamed_i         = Unsigned.UInt32.to_int32 item_renamed
    let item_modified_i        = Unsigned.UInt32.to_int32 item_modified
    let item_finder_info_mod_i = Unsigned.UInt32.to_int32 item_finder_info_mod
    let item_change_owner_i    = Unsigned.UInt32.to_int32 item_change_owner
    let item_xattr_mod_i       = Unsigned.UInt32.to_int32 item_xattr_mod
    let item_is_file_i         = Unsigned.UInt32.to_int32 item_is_file
    let item_is_dir_i          = Unsigned.UInt32.to_int32 item_is_dir
    let item_is_symlink_i      = Unsigned.UInt32.to_int32 item_is_symlink
    let item_is_hardlink_i     = Unsigned.UInt32.to_int32 item_is_hardlink
    let item_is_last_hardlink_i= Unsigned.UInt32.to_int32 item_is_last_hardlink

    let to_uint32 {
      must_scan_subdirs;
      event_ids_wrapped;
      history_done;
      root_changed;
      mount;
      unmount;
      own_event;
      item_created;
      item_removed;
      item_inode_meta_mod;
      item_renamed;
      item_modified;
      item_finder_info_mod;
      item_change_owner;
      item_xattr_mod;
      item_type;
      item_is_last_hardlink;
    } = Unsigned.UInt32.of_int32 (
      (match must_scan_subdirs with
       | None -> 0_l
       | Some { user; kernel } ->
         must_scan_subdirs_i |||
         (??> user user_dropped_i) |||
         (??> kernel kernel_dropped_i)
      ) |||
      (??> event_ids_wrapped event_ids_wrapped_i) |||
      (??> history_done history_done_i) |||
      (??> root_changed root_changed_i) |||
      (??> mount mount_i) |||
      (??> unmount unmount_i) |||
      (??> own_event own_event_i) |||
      (??> item_created item_created_i) |||
      (??> item_removed item_removed_i) |||
      (??> item_inode_meta_mod item_inode_meta_mod_i) |||
      (??> item_renamed item_renamed_i) |||
      (??> item_modified item_modified_i) |||
      (??> item_finder_info_mod item_finder_info_mod_i) |||
      (??> item_change_owner item_change_owner_i) |||
      (??> item_xattr_mod item_xattr_mod_i) |||
      (match item_type with
       | None -> 0_l
       | Some File    -> item_is_file_i
       | Some Dir     -> item_is_dir_i
       | Some Symlink -> item_is_symlink_i
       | Some Hardlink-> item_is_hardlink_i
      ) |||
      (??> item_is_last_hardlink item_is_last_hardlink_i)
    )

    let must_scan_subdirs_of_uint32 i =
      if ??< i must_scan_subdirs_i
      then Some {
        user = ??< i user_dropped_i;
        kernel = ??< i kernel_dropped_i;
      } else None

    let item_type_of_uint32 i =
      if ??< i item_is_file_i
      then Some File
      else if ??< i item_is_dir_i
      then Some Dir
      else if ??< i item_is_symlink_i
      then Some Symlink
      else if ??< i item_is_hardlink_i
      then Some Hardlink
      else None

    let of_uint32 i =
      let i = Unsigned.UInt32.to_int32 i in
      {
        must_scan_subdirs     = must_scan_subdirs_of_uint32 i;
        event_ids_wrapped     = ??< i event_ids_wrapped_i;
        history_done          = ??< i history_done_i;
        root_changed          = ??< i root_changed_i;
        mount                 = ??< i mount_i;
        unmount               = ??< i unmount_i;
        own_event             = ??< i own_event_i;
        item_created          = ??< i item_created_i;
        item_removed          = ??< i item_removed_i;
        item_inode_meta_mod   = ??< i item_inode_meta_mod_i;
        item_renamed          = ??< i item_renamed_i;
        item_modified         = ??< i item_modified_i;
        item_finder_info_mod  = ??< i item_finder_info_mod_i;
        item_change_owner     = ??< i item_change_owner_i;
        item_xattr_mod        = ??< i item_xattr_mod_i;
        item_type             = item_type_of_uint32 i;
        item_is_last_hardlink = ??< i item_is_last_hardlink_i;
      }

    let typ = view ~read:of_uint32 ~write:to_uint32 t

  end

  module EventId = struct

    type t =
      | Now
      | Since of Unsigned.UInt64.t

    let of_uint64 i =
      if i = T.EventId.since_now
      then Now
      else Since i

    let to_uint64 = function
      | Now -> T.EventId.since_now
      | Since i -> i

    let typ = view ~read:of_uint64 ~write:to_uint64 T.EventId.t

  end

  module Callback = struct

    type t = string -> EventFlags.t -> EventId.t -> unit

    let void_string_typ = view
        ~read:(coerce (ptr void) (ptr string))
        ~write:(coerce (ptr string) (ptr void))
        (ptr void)

    (* typedef void ( *FSEventStreamCallback )(
         ConstFSEventStreamRef streamRef,
         void *clientCallBackInfo,
         size_t numEvents,
         void *eventPaths,
         const FSEventStreamEventFlags eventFlags[],
         const FSEventStreamEventId eventIds[]);
    *)
    let cstring_typ =
      Foreign.funptr ~runtime_lock:true ~name:"FSEventStreamCallback" (
        const_typ @->
        ptr void @->
        size_t @->
        void_string_typ @->
        typedef (ptr EventFlags.typ) "const FSEventStreamEventFlags *" @->
        typedef (ptr T.EventId.t) "const FSEventStreamEventId *" @->
        returning void
      )

    let to_cstring_typ fn _stream _info num_events paths flags ids =
      let n = Unsigned.Size_t.to_int num_events in
      let paths = CArray.from_ptr paths n in
      let flags = CArray.from_ptr flags n in
      let ids   = CArray.from_ptr ids   n in
      for i = 0 to n - 1 do
        let id = EventId.of_uint64 (CArray.get ids i) in
        fn (CArray.get paths i) (CArray.get flags i) id
      done

  end

  module Context = struct

    type 'a t = {
      version : int;
      info : 'a;
      retain : Cf.Allocator.retain_callback_t;
      release : Cf.Allocator.release_callback_t;
      copy_description : Cf.Allocator.copy_description_callback_t;
    }

    let typ = typedef (ptr void) "FSEventStreamContext"

  end

  module PathList = Cf.Array.List.Make(Cf.String.String)

  (* extern FSEventStreamRef FSEventStreamCreate(
       CFAllocatorRef allocator,
       FSEventStreamCallback callback,
       FSEventStreamContext *context,
       CFArrayRef pathsToWatch,
       FSEventStreamEventId sinceWhen,
       CFTimeInterval latency,
       FSEventStreamCreateFlags flags
     ); *)
  let create = F.(foreign "FSEventStreamCreate" (
    ptr_opt void @->
    Callback.cstring_typ @->
    ptr_opt Context.typ @->
    PathList.typ @->
    EventId.typ @->
    Cf.TimeInterval.typ @->
    CreateFlags.typ @->
    returning typ
  ))

  (* extern FSEventStreamEventId FSEventStreamGetLatestEventId(
       ConstFSEventStreamRef streamRef
     ); *)
  let get_latest_event_id = F.(foreign "FSEventStreamGetLatestEventId" (
    typ @-> returning EventId.typ
  ))

  (* extern void FSEventStreamScheduleWithRunLoop(
       FSEventStreamRef   streamRef,
       CFRunLoopRef       runLoop,
       CFStringRef        runLoopMode
     ); *)
  let schedule_with_run_loop = F.(foreign "FSEventStreamScheduleWithRunLoop" (
    typ @->
    Cf.RunLoop.typ @->
    Cf.RunLoop.Mode.typ @->
    returning void
  ))

  (* extern Boolean FSEventStreamStart(
       FSEventStreamRef streamRef
     ); *)
  let start = F.(foreign "FSEventStreamStart" (
    typ @-> returning bool
  ))

  (* extern void FSEventStreamFlushSync(
       FSEventStreamRef streamRef
     ); *)
  let flush_sync = F.(foreign "FSEventStreamFlushSync" (
    typ @-> returning void
  ))

  (* extern void FSEventStreamStop(
       FSEventStreamRef streamRef
     ); *)
  let stop = F.(foreign "FSEventStreamStop" (
    typ @-> returning void
  ))

  (* extern void FSEventStreamInvalidate(
       FSEventStreamRef streamRef
     ); *)
  let invalidate = F.(foreign "FSEventStreamInvalidate" (
    typ @-> returning void
  ))

  (* extern void FSEventStreamRelease(
       FSEventStreamRef streamRef
     ); *)
  let release = F.(foreign "FSEventStreamRelease" (
    typ @-> returning void
  ))

  (* extern CF_RETURNS_RETAINED CFArrayRef FSEventStreamCopyPathsBeingWatched(
       ConstFSEventStreamRef streamRef
     ); *)
  let copy_paths_being_watched =
    F.(foreign "FSEventStreamCopyPathsBeingWatched" (
      const_typ @-> returning PathList.typ
    ))

end
