(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
 *)

[@@@ocaml.deprecated "This module will be retired with MirageOS 4.0, use Mirage_kv instead!"]

(** MirageOS signatures for filesystem devices

    {e v3.0.1 } *)

(** {2 Mirage_fs} *)

(** The type for FS errors. *)
type error = [
  | `Is_a_directory      (** Cannot read or write the contents of a directory *)
  | `No_directory_entry  (** Cannot find a directory entry *)
  | `Not_a_directory     (** Cannot create a directory entry in a file *)
]

val pp_error: error Fmt.t
(** [pp_error] is the pretty-printer for errors. *)

(** The type for FS write errors. *)
type write_error = [
  | error
  | `File_already_exists (** Cannot create a file with a duplicate name *)
  | `No_directory_entry  (** Something in the path doesn't exist *)
  | `No_space            (** No space left on the block device *)
]

val pp_write_error: write_error Fmt.t
(** [pp_write_error] is the pretty-printer for write errors. *)

type stat = {
  filename: string; (** Filename within the enclosing directory *)
  read_only: bool;  (** True means the contents are read-only *)
  directory: bool;  (** True means the entity is a directory; false means a file *)
  size: int64;      (** Size of the entity in bytes *)
}
(** The type for Per-file/directory statistics. *)

(** {2 Filesystem} *)
module type S = sig

  type error = private [>
    | `Is_a_directory
    | `No_directory_entry
    | `Not_a_directory
  ]
  (** The type for errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type write_error = private [>
    | `Is_a_directory
    | `No_directory_entry
    | `Not_a_directory
    | `File_already_exists
    | `No_directory_entry
    | `No_space
  ]
  (** The type for FS write errors. *)

  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  include Mirage_device.S

  val read: t -> string -> int -> int ->
    (Cstruct.t list, error) result Lwt.t
  (** [read t key offset length] reads up to [length] bytes from the
      value associated with [key]. If less data is returned than
      requested, this indicates the end of the value. *)

  val size: t -> string -> (int64, error) result Lwt.t
  (** Get the value size. *)

  val create: t -> string -> (unit, write_error) result Lwt.t
  (** [create t path] creates an empty file at [path]. If [path]
      contains directories that do not yet exist, [create] will
      attempt to create them. *)

  val mkdir: t -> string -> (unit, write_error) result Lwt.t
  (** [mkdir t path] creates an empty directory at [path].  If [path]
      contains intermediate directories that do not yet exist, [mkdir]
      will create them.  If a directory already exists at [path],
      [mkdir] returns [`Ok ()] and takes no action. *)

  val destroy: t -> string -> (unit, write_error) result Lwt.t
  (** [destroy t path] removes a [path] (which may be a file or an
      empty directory) on filesystem [t]. *)

  val stat: t -> string -> (stat, error) result Lwt.t
  (** [stat t path] returns information about file or directory at
      [path]. *)

  val listdir: t -> string -> (string list, error) result Lwt.t
  (** [listdir t path] returns the names of files and subdirectories
      within the directory [path]. *)

  val write: t -> string -> int -> Cstruct.t ->
    (unit, write_error) result Lwt.t
  (** [write t path offset data] writes [data] at [offset] in file
      [path] on filesystem [t].

      If [path] contains directories that do not exist, [write] will
      attempt to create them.  If [path] already exists, [write] will
      overwrite existing information starting at [off].*)

end

(** Consider a filesystem device as a key/value read-only store. *)
module To_KV_RO (FS: S): sig
  include Mirage_kv.RO with type t = FS.t
  val connect : FS.t -> t Lwt.t
end
