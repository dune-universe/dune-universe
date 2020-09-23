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

(** Extension of the stdlib String module *)

(** [for_all fn s] tests whether every characters of [s] verify [fn] *)
val for_all : (char -> bool) -> string -> bool

(** [exists fn s] tests whether at least a character of [s] verifies
    [fn] *)
val exists : (char -> bool) -> string -> bool

(** [before s pos] returns the substring before [pos] (the character
    at position [pos] is not included) *)
val before : string -> int -> string

(** [after s pos] returns the substring after [pos] (the character at
    position [pos] is not included) *)
val after : string -> int -> string

(** [starts_with s ~prefix] checks whether [s] starts with [prefix] *)
val starts_with : string -> prefix:string -> bool

(** [ends_with s ~suffix] checks whether [s] ends with [suffix] *)
val ends_with : string -> suffix:string -> bool

(** [cut pos s] returns the substrings of [s] before and after
    the position [pos]. The character at position
    [pos] is not included *)
val cut : string -> int -> string * string

(** [cut_at c s] returns the substring of [s] before and after
    the position first occurence of character [c]. *)
val cut_at : string -> char -> string * string

(** [rcut_at c s] returns the substring of [s] before and after
    the position last occurence of character [c]. *)
val rcut_at : string -> char -> string * string

(** [split s c] splits the string [s] on characters [c],
    starting from the left.
   Invariant:
    String.concat (String.make 1 c) (split s c) =  s

*)
val split : string -> char -> string list

(** [split s c] splits the string [s] on characters [c],
   starting from the left, removing empty sub strings. *)
val split_simplify : string -> char -> string list

(* [chop_prefix s ~prefix] returns [None] if the string [s] does not
   start with [prefix], and otherwise, it returns [Some suffix] such
   that [s = prefix ^ suffix]. Only in 0.2.2. *)
val chop_prefix : string -> prefix:string -> string option

(* [chop_suffix s ~suffix] returns [None] if the string [s] does not
   end with [suffix], and otherwise, it returns [Some prefix] such
   that [s = prefix ^ suffix]. Only in 0.2.2. *)
val chop_suffix : string -> suffix:string -> string option
