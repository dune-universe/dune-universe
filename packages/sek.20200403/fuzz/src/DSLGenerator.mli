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

open DSLSyntax

module Make (I : sig

  (* The instruction generator is parameterized by an instance of the
     interpreter. This allows the generator to consult the interpreter's
     environment. This makes sense because we interleave the generation and
     execution of instructions, that is, we have access to the result of
     evaluating the previous instructions as we generate the next
     instruction. *)

  (* See [DSLInterpreter] for comments. *)

  type env

  val nevars: env -> int
  val npvars: env -> int
  val nseqvars: env -> int
  val evars: env -> var list
  val pvars: env -> var list
  val seqvars: env -> var list

  val length: env -> var -> int

end) : sig

  (* As a side effect, this functor application instantiates a fresh element
     generator. *)

  (* [instruction elem env] generates an instruction. The runtime environment
     can be used to find out how many ephemeral / persistent variables are in
     scope. *)

  val instruction: I.env -> instruction

end
