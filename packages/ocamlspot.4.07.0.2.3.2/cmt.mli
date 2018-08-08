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

(* cmt file conventions *)

open Cmt_format

val source_path : cmt_infos -> string option
(** returns the full path of the source file *)

val of_path : string -> string
(** get the corresponding cmt/cmti path name of the given file name:
    of_path "dir/x.cmi" = "dir/x.cmti"
    of_path "dir/x.ml"  = "dir/x.cmt"
*)

val is_opt : cmt_infos -> bool
(** Guess the cmt is created by opt(native code) compilation *)

val reset_env_cache : unit -> unit

val recover_env : Env.t -> Env.t
(** Type environments in cmt are simplified and just have env summaries.
    If we want the real environment, we need to recover it from the summary. *)
