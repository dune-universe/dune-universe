(**************************************************************************)
(*                                                                        *)
(*   Typerex Libraries                                                    *)
(*                                                                        *)
(*   Copyright 2011-2017 OCamlPro SAS                                     *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Signatures exported by other modules *)

open [@ocaml.warning "-33"] EzCompat

(** This module provides functions to read and write complete files
    from and to strings, or working on file lines. *)
module type CONTENT_OPERATIONS = sig

  type in_file
  (** The type of a file from which we read:
      [in_channel] for [FileChannel], [string] for [FileString] and
      [FileGen.t] for [FileGen].
    *)

  type out_file
  (** The type of a file to which we write:
      [out_channel] for [FileChannel], [string] for [FileString] and
      [FileGen.t] for [FileGen].
 *)

(*

   Converting file contents to string, back and forth.

*)

  (** [read_file file] returns the full content of [file]. If the file
     is opened, it is opened in binary mode, no conversion is
     applied.*)
  val read_file : in_file -> string

  (** [write_file file content] creates file [file] with content
     [content].  If the file is opened, it is opened in binary mode,
     no conversion is applied.*)
  val write_file : out_file -> string -> unit

  (** [read_subfile file pos len] returns a string containing [len]
     bytes read from file [file] at pos [pos]. If the file is opened,
     it is opened in binary mode. Raises [End_of_file] if the file is
     too short.  *)
  val read_subfile : in_file -> int -> int -> string

(*

   Converting file contents to lines, back and forth.

*)

  (** [read_lines file] returns the content of [file] as an array of
     lines. If the file is opened, it is opened in text mode. *)
  val read_lines :  in_file -> string array

  (** [read_lines_to_list file] returns the content of [file] as a
     list of lines. If the file is opened, it is opened in text
     mode. *)
  val read_lines_to_list :  in_file -> string list

  (** [write_lines file lines] creates the file [file] from an array of
     lines, using [FileChannel.output_line] for each line. *)
  val write_lines :  out_file -> string array -> unit

  (** [write_lines file lines] creates the file [file] from a list of
     lines, using [FileChannel.output_line] for each line. *)
  val write_lines_of_list :  out_file -> string list -> unit

  (** [read_sublines file pos len] returns at most [len] lines of the
     file [file], starting at line [pos]. It differs from [read_subfile]
     in that it will not raise any exception if the file is too
     short. Note that it reads the file from beginning everytimes. *)
  val read_sublines : in_file -> int -> int -> string array

  (** Same as [read_sublines], but returns a list of strings. *)
  val read_sublines_to_list : in_file -> int -> int -> string list



  (*

   Iterators on chunks and lines.

  *)


  (** [iter_blocks f file] reads the content of file [file], and calls
     [f buffer len] on each chunk. The [buffer] is reused, and only
     the first [len] bytes are from the file. Chunks have a maximal
     size of 32768.  *)
  val iter_blocks : (Bytes.t -> int -> unit) -> in_file -> unit

  (** [iter_lines f file] calls [f line] on all the lines [line] of
   the file [file]. *)
  val iter_lines : (string -> unit) -> in_file -> unit

  (** [iteri_lines f file] calls [f line_num line] on every line [line]
     of the file [file], with [line_num] the line number, starting with
     line 0. *)
  val iteri_lines : (int -> string -> unit) -> in_file -> unit


  (** [copy_file src dst] copy all the content remaining in file [src]
     to file [dst]. *)
  val copy_file : in_file -> out_file -> unit


  (**/**)
  (* Obsolete functions *)
  val string_of_file : in_file -> string
  val lines_of_file : in_file -> string array
  val file_of_string : out_file -> string -> unit
  val string_of_subfile : in_file -> int -> int -> string
  val file_of_lines : out_file -> string array -> unit

  (* Optimized functions, not documented *)
  val read_lines_to_revlist :  in_file -> string list
  (**/**)

end


(** This modules provides functions to create directories, read, iter
   or remove directories, recursively or not.*)
module type DIRECTORY_OPERATIONS = sig

  type t
    (** Type of a filename. It is [string] in [FileString] and
        [FileGen.t] in [FileGen]. *)

  type selector
    (** Type of the value used to create filters when iterating on
        files and directories. *)

  exception NotADirectory of t
      (** This exception is raised when one of the following functions is
          called with a non-directory argument *)

  (** [make_dir ?mode ?p filename] creates a directory [filename], if it
      does not already exist. It fails with [NotADirectory] if the file
      already exists, but is not a directory.
      The [mode] argument is the Unix permissions (0o755 by default).
      The [p] argument controls whether parents directories should be
      created as well, if they don't exist, instead of failing. *)
  val make_dir : ?mode:int -> ?p:bool -> t -> unit

  (* selector to iter only on one directory *)
  val onedir : selector

  (* selector to iter recursively on a directory *)
  val recdir : selector

  (** [remove_dir ?all filename] removes directory [filename], or
     complains the [NotADirectory] if it does not exist. The [all]
     argument controls whether the function should recursively remove
     all files and sub-directories included as well. If [glob] is
     specified, it is called to select files to remove based on their
     basename, and the directories are not deleted even if [all] is
     [true].*)
  val remove_dir : ?all:bool -> ?glob:string -> t -> unit

  (** [select ?deep ?dft ?glob ?filter_rec ?filter_fun ?follow_links
     ?error ()] creates a selctor to customize a file iterator.

      The [deep] and [dft] arguments controls whether function should
     recurse (false by default) in sub-directories.  If [deep] is
     [true], and [~dft] is not specified, the files are listed in
     breadth-first mode ([a,b,a/x,b/x,a/x/y] for example).  If [~dft]
     is [`Before], the files are listed in depth-first mode, and the
     ancestors are before their children. If [~dft] is [`After], the
     are after their children.

      The [glob], [path] and [ignore] arguments can be used to filter
     the files with regular expressions. [glob] is used to choose the
     files to keep based on their basenames, it is not used for the
     recursion itself. [path] is used to choose the files to keep
     based on the path, it is not used for the recursion
     itself. [ignore] is used to choose the files to ignore, based on
     their paths, both during the iteration and the recursion.

      The [kinds] argument can be used to restrict the files to the
     ones of a particular kind.

      The [filter] argument is used to select the files to keep.
     [filter for_rec path] should return [true] if the file is ok,
     [false] otherwise. [for_rec] is [true] if [filter] is called to
     check a directory for recursion, [false] if it is called for the
     iteration.

      The [follow_links] argument is used to decide if a link to
     directory should be followed (when [deep] is also set).

      The [error] argument is called when an error occurs, with [error
     exn filename]. The default behavior is to ignore errors.  *)
  val select :
    ?deep:bool ->
    ?dft:[ `After | `Before ] ->
    ?glob:string -> (* check on basenames, not for recursion *)
    ?path:string -> (* check on paths, not for recursion *)
    ?ignore:string -> (* ignore these paths, always *)
    ?kinds: Unix.file_kind list ->
    ?filter:(bool -> string -> bool) ->
    ?follow_links:bool ->
    ?error:(exn -> t -> unit) -> unit -> selector

  (** [read_dir ?select dir] returns the files contained in the
     directory [dir], recursively. Filenames start from the root of
     the given directory, i.e. a full filename would be [dir / file].

     In a directory, files are sorted in lexicographical order of
     their names. *)
  val read_dir : ?select:selector -> t -> string array

  (** Same as [read_dir], but returns a list instead of an array *)
  val read_dir_to_list : ?select:selector -> t -> string list

  (** Same as [read_dir], but calls a function on every file and
     directory. It is not equivalent to using [read_dir] and then
     itering on the result, as [iter_dir] the function is called
     during the traversal, not after, so concurrent modifications to
     the directory might become visible.  *)
  val iter_dir :
    ?select:selector -> t -> f:(string -> unit) -> unit

  (** [iterator ?select dir] creates an iterator on directory [dir].
      The iterator is a function that returns [None] when finished,
      or [Some file] with the next file to iter on.
  *)
  val iterator : ?select:selector -> t -> (unit -> string option)

  (** [make_select f] transforms [f], a function that takes an
     optional selector, into a function that builds the selector on
     the fly.

      For example:
      let () =
         EzFile.make_select EzFile.iter_dir ~deep:true
            ~f:(fun path -> ...) dirname

      which is equivalent to:
      let () =
         let select = EzFile.select ~deep:true () in
         EzFile.iter_dir ~select
            ~f:(fun path -> ...) dirname
  *)

  val make_select :
    (?select:selector -> t -> 'a) ->
    ?deep:bool ->
    ?dft:[ `After | `Before ] ->
    ?glob:string -> (* check on basenames, not for recursion *)
    ?path:string -> (* check on paths, not for recursion *)
    ?ignore:string -> (* ignore these paths, always *)
    ?kinds: Unix.file_kind list ->
    ?filter:(bool -> string -> bool) ->
    ?follow_links:bool ->
    ?error:(exn -> t -> unit) ->
    t ->
    'a

  (** [mkdir filename mode] simply creates the directory [filename] with
      permissions [mode]. *)
  val mkdir : t -> int -> unit

  (** [readdir filename] returns the files contained in directory
     [filename] as an array of strings. The strings are sorted in
     lexicographical order. *)
  val readdir : t -> string array

  (** [rmdir filename] removes directory [filename], or fails if it
    does not exist or is not a directory. *)
  val rmdir : t -> unit

(**/**)
  val safe_mkdir : t -> unit
(**/**)

end

module type FILENAME_OPERATIONS = sig

  type t

(*

Operations on filenames

*)

  val concat : t -> t -> t
  val is_absolute : t -> bool
  val is_relative : t -> bool
  val is_implicit : t -> bool

  val add_suffix : t -> string -> t
  val check_suffix : t -> string -> bool

  (* [cut_extension file] returns a pair [ basename, extensions ],
     where [extensions] is the string of all extensions in lowercase
     (without the leading dot), and [basename] is the file basename
     without the extensions.  *)
  val cut_extension : t -> string * string

  (* [cut_extensions file] returns a pair [ basename, extensions ],
     where [extensions] is the list of extensions in lowercase
     (without dots), and [basename] is the file basename without the
     extensions.  *)
  val cut_extensions : t -> string * string list

  val basename : t -> string
  val dirname : t -> t
  val add_path : t -> string -> t
  val add_basename : t -> string -> t
  val add_basenames : t -> string list -> t

  val current_dir_name : t

(*

  Standard operations on files

*)


  val open_in : t -> in_channel
  val open_out : t -> out_channel
  val open_in_bin : t -> in_channel
  val open_out_bin : t -> out_channel
  val open_fd :
    t -> MinUnix.open_flag list -> MinUnix.file_perm -> MinUnix.file_descr
  val temp_file : t -> string -> t

  val with_in : t -> (in_channel -> unit) -> unit
  val with_in_bin : t -> (in_channel -> unit) -> unit
  val with_out : t -> (out_channel -> unit) -> unit
  val with_out_bin : t -> (out_channel -> unit) -> unit

  val exists : t -> bool
  val getcwd : unit -> t
  val size : t -> int
  val is_directory : t -> bool
  val is_link : t -> bool

  val remove : t -> unit
  val rename : t -> t -> unit

  val stat : t -> MinUnix.stats
  val lstat : t -> MinUnix.stats


  module OP : sig
    (* concatenate ('/' must be the only file separator in the string) *)
    val (//) : t -> string -> t
  end

(*

Copying files

*)

(*
  (* [safe_mkdir dirname] creates a directory [dirname], potentially
     creating any parent directory. A [~mode] argument can be
     provided, otherwise it is assumed to be 0o755. [safe_mkdir] can
     fail for wrong permissions, or if a directory name is already
     used by another kind of files.*)
  val safe_mkdir : ?mode:int -> t -> unit
*)

  (* [copy_rec src dst] creates [dst] as a copy of [src], even
     if [src] is a directory. *)
  val copy_rec : t -> t -> unit

  (* [uncopy_rec src dst] removes from [dst] everything that has the
     same name as in [src]. Can be seen as the inverse operation of
     [copy_rec]. *)
  val uncopy_rec : t -> t -> unit

(** [find_in_path path filename] searches a file in a list of directories. *)
  val find_in_path : string list -> string -> t


end

module type FILE_OPERATIONS = sig

  include FILENAME_OPERATIONS

  include (CONTENT_OPERATIONS with type in_file := t and type out_file := t)
  include (DIRECTORY_OPERATIONS with type t := t)

end
