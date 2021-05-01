(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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

module C(F: Cstubs.Types.TYPE) = struct

  let const = F.constant

  let lift = F.lift_typ

  module CreateFlags = struct

    let t = F.uint32_t

    let prefix = "kFSEventStreamCreateFlag"

    let use_cf_types = const (prefix^"UseCFTypes") t
    let no_defer     = const (prefix^"NoDefer") t
    let watch_root   = const (prefix^"WatchRoot") t
    let ignore_self  = const (prefix^"IgnoreSelf") t
    let file_events  = const (prefix^"FileEvents") t
    let mark_self    = const (prefix^"MarkSelf") t

  end

  module EventFlags = struct

    let t = F.typedef F.uint32_t "FSEventStreamEventFlags"

    let prefix = "kFSEventStreamEventFlag"

    let must_scan_subdirs    = const (prefix^"MustScanSubDirs") t
    let user_dropped         = const (prefix^"UserDropped") t
    let kernel_dropped       = const (prefix^"KernelDropped") t
    let event_ids_wrapped    = const (prefix^"EventIdsWrapped") t
    let history_done         = const (prefix^"HistoryDone") t
    let root_changed         = const (prefix^"RootChanged") t
    let mount                = const (prefix^"Mount") t
    let unmount              = const (prefix^"Unmount") t
    let own_event            = const (prefix^"OwnEvent") t
    let item_created         = const (prefix^"ItemCreated") t
    let item_removed         = const (prefix^"ItemRemoved") t
    let item_inode_meta_mod  = const (prefix^"ItemInodeMetaMod") t
    let item_renamed         = const (prefix^"ItemRenamed") t
    let item_modified        = const (prefix^"ItemModified") t
    let item_finder_info_mod = const (prefix^"ItemFinderInfoMod") t
    let item_change_owner    = const (prefix^"ItemChangeOwner") t
    let item_xattr_mod       = const (prefix^"ItemXattrMod") t
    let item_is_file         = const (prefix^"ItemIsFile") t
    let item_is_dir          = const (prefix^"ItemIsDir") t
    let item_is_symlink      = const (prefix^"ItemIsSymlink") t
    let item_is_hardlink     = const (prefix^"ItemIsHardlink") t
    let item_is_last_hardlink= const (prefix^"ItemIsLastHardlink") t

  end

  module EventId = struct

    let t = F.typedef F.uint64_t "FSEventStreamEventId"

    let prefix = "kFSEventStreamEventId"

    let since_now = const (prefix^"SinceNow") t

  end

  module Context = struct
    type t

    let t : t Ctypes_static.structure F.typ = F.structure "FSEventStreamContext"

    let version = F.field t "version" (lift Cf.Index.typ)
    let info    = F.field t "info"    F.(ptr void)
    let retain  = F.field t "retain"  (lift Cf.Allocator.retain_callback_typ)
    let release = F.field t "release" (lift Cf.Allocator.release_callback_typ)
    let copy_description =
      F.field t "copyDescription"
        (lift Cf.Allocator.copy_description_callback_typ)

    let () = F.seal t

  end

end
