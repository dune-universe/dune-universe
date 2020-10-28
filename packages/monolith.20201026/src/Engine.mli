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

(* See Monolith.mli for documentation. *)
type document = PPrint.document

(* See Monolith.mli for documentation. *)
val main : ?prologue:(unit -> unit) -> int -> unit

(* See Monolith.mli for documentation. *)
exception PleaseBackOff
exception Unimplemented
