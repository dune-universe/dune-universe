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

open Eq

(* -------------------------------------------------------------------------- *)

(* The type ['a postcondition] describes a reference function that verifies
   a result of type ['a] produced by the candidate implementation. *)

type 'a postcondition =
  'a -> diagnostic

(* The postcondition must determine whether the candidate result is valid or
   invalid. In the latter case, it must produce a piece of OCaml code, which
   is parameterized with one variable [observed], and which explains why this
   input is unacceptable. *)

and diagnostic =
  | Valid
  | Invalid of (PPrint.document -> PPrint.document)

(* -------------------------------------------------------------------------- *)

(* A value of type ['a ty] is a runtime representation of the type ['a]. *)

(* These runtime type representations are used to guide the interpreter. *)

(* The type language includes simple types (base types, pairs, functions) as
   well as a special type of non-deterministic computations, which receives
   special treatment by the interpreter. *)

type _ ty =
  | TyBase :
      'a Tag.tag -> 'a ty
  | TyPair :
      'a ty * 'b ty -> ('a * 'b) ty
  | TyArrow :
      'a ty * 'b ty -> ('a -> 'b) ty
  | TyNondet :
      'a ty -> 'a postcondition ty

(* -------------------------------------------------------------------------- *)

(* A runtime type equality test. *)

exception RuntimeTypeError = Tag.RuntimeTagError

let rec equal : type a1 a2 . a1 ty -> a2 ty -> (a1, a2) eq
= fun ty1 ty2 ->
  match ty1, ty2 with
  | TyBase tag1, TyBase tag2 ->
      Tag.equal tag1 tag2
  | TyPair (ty1a, ty1b), TyPair (ty2a, ty2b) ->
      let Eq = equal ty1a ty2a in
      let Eq = equal ty1b ty2b in
      Eq
  | TyArrow (ty1a, ty1b), TyArrow (ty2a, ty2b) ->
      let Eq = equal ty1a ty2a in
      let Eq = equal ty1b ty2b in
      Eq
  | TyNondet ty1, TyNondet ty2 ->
      let Eq = equal ty1 ty2 in
      Eq
  | _, _ ->
      raise RuntimeTypeError

(* -------------------------------------------------------------------------- *)

(* A value of type [any] is a pair of a runtime representation of some type
   ['a] and a value of the type ['a]. *)

type any =
  | Any :
      'a ty * 'a -> any

(* -------------------------------------------------------------------------- *)

(* A constructor function for the type [any]. *)

let wrap (type a) (ty : a ty) (v : a) : any =
  Any (ty, v)

(* -------------------------------------------------------------------------- *)

(* [unwrap] maps a runtime type representation [ty1] of type ['a ty] and a
   value [v2] of type [any] to a raw value of type ['a]. It raises the
   exception [RuntimeTypeError] if [v2] carries an unexpected type
   representation. This feature is exploited by the engine to restrict the
   runtime environment to variables of a certain type. *)

let unwrap (type a) (ty1 : a ty) (v2 : any) : a =
  let Any (ty2, v2) = v2 in
  let Eq = equal ty1 ty2 in
  v2

(* -------------------------------------------------------------------------- *)

(* The functor [NewTy] extends the type ['a ty] with a new case, which serves
   as a runtime representation of the type [t] provided by the caller. *)

module NewTy (X : sig type t end) = struct
  include Tag.NewTag(X)
  let ty = TyBase Tag
end

(* -------------------------------------------------------------------------- *)

(* Constructing and deconstructing pairs. *)

let pair (v1 : any) (v2 : any) : any =
  let Any (ty1, v1) = v1 in
  let Any (ty2, v2) = v2 in
  Any (TyPair (ty1, ty2), (v1, v2))

let unpair (v : any) : any * any =
  let Any (ty, v) = v in
  match ty with
  | TyPair (ty1, ty2) ->
      let (v1, v2) = v in
      Any (ty1, v1), Any (ty2, v2)
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* Applying functions. *)

let apply (v1 : any) (v2 : any) : any =
  let Any (ty1, v1) = v1 in
  match ty1 with
  | TyArrow (domain, codomain) ->
      Any (codomain, v1 (unwrap domain v2))
  | _ ->
      assert false
