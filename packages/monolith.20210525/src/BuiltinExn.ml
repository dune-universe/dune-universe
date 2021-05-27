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

open Spec

(* -------------------------------------------------------------------------- *)

(* A concrete type: [exn]. *)

(* [exn] is used in the definition of the combinator [(^!>)]. *)

(* We view [exn] as a deconstructible type only, because exceptions
   are always a result that is observed, never an argument. *)

(* We equip the type [exn] with an extensible notion of equality. Initially,
   it is the default equality; this implies that when the candidate and
   reference implementations both raise an exception, they are expected to
   raise exactly the same exception. The user can override this with new
   cases. *)

let exn_eq_hook : (exn -> exn -> bool) ref =
  ref (=)

let () =
  GlobalState.register_ref exn_eq_hook

let override_exn_eq update =
  exn_eq_hook := update !exn_eq_hook

let exn_eq e1 e2 =
  !exn_eq_hook e1 e2

let print =
  Print.exn

let equal =
  exn_eq, Code.infix "=exn=" (* not a value; monomorphic *)

let exn =
  SpecDeconstructible { equal; print }
