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

module CreateFlags : sig

  type t = {
    use_cf_types : bool;
    no_defer : bool;
    watch_root : bool;
    ignore_self : bool;
    file_events : bool;
    mark_self : bool;
  }

  val detailed_interactive : t

end

module EventFlags : sig

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

  val to_string : t -> string

  val to_string_one_line : t -> string

  val zero : t
end

module EventId : sig
  type t =
    | Now
    | Since of Unsigned.UInt64.t

  val of_uint64 : Unsigned.UInt64.t -> t

  val to_uint64 : t -> Unsigned.UInt64.t

  val typ : t Ctypes.typ

  val min : t -> t -> t

  val max : t -> t -> t

  val compare : t -> t -> int
end

type t

type callback = string -> EventFlags.t -> EventId.t -> unit

val create :
  ?since:EventId.t -> float -> CreateFlags.t -> callback -> string list -> t

val get_latest_event_id : t -> EventId.t

val schedule_with_run_loop : t -> Cf.RunLoop.t -> Cf.RunLoop.Mode.t -> unit

val start : t -> bool

val flush_sync : t -> unit

val stop : t -> unit

val invalidate : t -> unit

val release : t -> unit

val copy_paths_being_watched : t -> string list

module Types = Types
module Types_detected = Types_detected
module Bindings = Bindings
module Generated = Generated
