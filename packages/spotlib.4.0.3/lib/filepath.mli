(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2012 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

(* File path normalization *)

type os = 
  | Unix   (** We love *)
  | Win32  (** We hate *)
  | Cygwin (** A failed effort of reconcillation *)

val os : os
(** The actual OS the program is running on *)

type t
(** The type of path *)

val is_prefix : t -> t -> string list option
(** [is_prefix a b] checks [a] is a perfix of [b]. 

    For example,

    [is_prefix /a/b/c /a/b/c/d/e = Some ["d"; "e"]] 
    [is_prefix a/b/c a/b/c/d/e = Some ["d"; "e"]] 
*)

val of_string : os -> string -> t
(** Parsing. No normalization performed. *)

val to_string : t -> string
(** Printing *)

val normalize : t -> t
(** Normalize the path.

    * Drive name capitalization/uncapitalization
    * Use backslashes in Win32
    * Elimination of redundant /./ and /../, if possible.
*)

val is_absolute : t -> bool
val is_relative : t -> bool
val is_root : t -> bool

val dirbase : t -> t * string option
(** Split the path into its directory and base parts.

    If the path is the root, it returns None.

    It fails and raises [Invalid_argument "dirbase"],
    when the path ends with "..".
*)

val (^/) : t -> string -> t
(** Concatenation. 

    [t ^/ "hello/world"]

    [t ^/ x] raises [Invalid_argument "(^/)"] when [x] is a string
    of an absolute path.
*)

val concats : t -> string list -> t
(** Concatenation+

    [concats t ["hello"; "world"; "good/bye"]]

    should equal to

    [t ^/ "hello" ^/ "world" ^/ "good" ^/ "bye"]

    It raises [Invalid_argument "(^/)"] when the path components
    contain an absolute path.
*) 

val parent : t -> t

val wrap : os -> (t -> t) -> string -> string

val test : unit -> unit
