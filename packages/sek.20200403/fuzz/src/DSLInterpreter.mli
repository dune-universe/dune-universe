(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Signature
open DSLSyntax

(* A test program interpreter. *)

module Make (S : SEQUENCES) : sig

  (* The type of runtime environments. A runtime environment maps each
     variable in scope to a value of appropriate type. E.g., a variable that
     denotes an ephemeral sequence is mapped to an ephemeral sequence. *)

  type env

  (* The empty environment. *)

  val empty: env

  (* [nevars] and [npvars] indicate how many ephemeral / persistent variables
     are in scope and available for use. [evars] and [pvars] are the lists of
     these variables. *) (* TODO update comment *)

  val nevars: env -> int
  val npvars: env -> int
  val nseqvars: env -> int
  val evars: env -> var list
  val pvars: env -> var list
  val seqvars: env -> var list

  (* [length env x] returns the length of the (ephemeral or persistent)
     sequence denoted by the variable [x]. *)

  val length: env -> var -> int

  (* [check_wf env] checks that every variable in scope is bound to a
     well-formed data structure. *)

  val check_wf : env -> unit

  (* [interpret env i] interprets the instruction [i] in the environment
     [env]. It returns the instruction [i], annotated with observations, and
     an updated environment which is in force after this instruction. *)

  val interpret: env -> instruction -> instruction * env

end
