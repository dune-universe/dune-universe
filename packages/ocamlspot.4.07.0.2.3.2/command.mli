(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

val escape : (char -> bool) -> string -> string
(** escapes each char in the string which matches the predicate by a backslash *)

val escape_for_shell : string -> string
(** escapes shell special chars *)

val shell : string list -> int
(** execute shell command by [Sys.command] *)
