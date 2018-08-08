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

(* expansion of expr/pattern by type *)

open Types

type t 

val expand : string list (* load_path *) -> Env.t -> type_expr -> t
(** Build expansion of the given type *)

val format_as_expr    : Format.formatter -> t -> unit
val format_as_pattern : Format.formatter -> t -> unit
(** Print expansion as an expression or a pattern *)

module EnvSummary : sig
  val format : Format.formatter -> Env.summary -> unit
  (** Debugging purpose. Show the summary of the environment *)
end
