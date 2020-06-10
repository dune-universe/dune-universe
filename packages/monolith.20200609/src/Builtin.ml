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

open Type
open Spec

(* -------------------------------------------------------------------------- *)

(* A built-in concrete type: [unit]. *)

module TyUnit =
  NewConcreteType(struct
    type t = unit
    let name = "unit"
    let print () = Print.unit
    let equal = None
  end)

let unit =
  TyUnit.spec &&& (fun () -> ())

let unit_value =
  Any (TyUnit.T.ty, ())

(* -------------------------------------------------------------------------- *)

(* A built-in concrete type: [int]. *)

(* We do not equip the type [int] with a default generator, because that
   would not make much sense; we have no idea what range is relevant. *)

module TyInt =
  NewConcreteType(struct
    type t = int
    let name = "int"
    let print = Print.int
    let equal = None
  end)

let int =
  TyInt.spec

let interval i j =
  int &&& Gen.interval i j

let interval_ i j =
  int &&& Gen.interval_ i j

let lt j =
  int &&& Gen.lt j

let le j =
  int &&& Gen.le j

let sequential () =
  int &&& Gen.sequential()

(* -------------------------------------------------------------------------- *)

(* A built-in concrete type: [bool]. *)

module TyBool =
  NewConcreteType(struct
    type t = bool
    let name = "bool"
    let print = Print.bool
    let equal = None
  end)

let bool =
  TyBool.spec &&& Gen.bool
