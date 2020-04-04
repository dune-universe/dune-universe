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

(* Comparing the reference implementation and the candidate implementation. *)

module Make (Reference : SEQUENCES) (Candidate : SEQUENCES) : sig

  (* [test fuel] generates a sequence of instructions and executes it under
     both the reference implementation and the candidate implementation,
     comparing their observable behavior. [fuel] is the maximum length of
     this instruction sequence. *)

  val test: int -> unit

end
