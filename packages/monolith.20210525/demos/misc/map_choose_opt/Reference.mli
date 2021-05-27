(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

include Signature.S with type key = int
type map = int t

(* [choose_opt] is nondeterministic. *)

(* It is specialized to integer keys and values because this allows us
   to print better diagnostic messages. *)

open Monolith
val choose_opt: int t -> (int * int) option nondet
