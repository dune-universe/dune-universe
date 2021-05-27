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

(* This module offers utility functions that can play a role in scenarios.
   This means that we must make them available to the user. A simple-minded
   approach would be to print their definitions as part of error scenarios.
   It seems preferable to just make them available as part of the Monolith
   API, so the user can type [#require "monolith";;] in the OCaml REPL and
   will have access to all functions in [Monolith.Support]. *)

(* We define [Sup] as a short name for [Monolith.Support] at the beginning
   of every scenario. This is done in [Engine.main]. *)

let support name =
  Code.constant ("Sup." ^ name)

(* -------------------------------------------------------------------------- *)

module Fun = struct

  let id x = x

  module Id = struct

    let appearance =
      support "Fun.id"

    (* The following is an optional optimization: when [id] is applied to at
       least one argument, we can perform beta-reduction on the fly, so that
       the application of [id] becomes invisible. *)

    let appearance =
      Code.custom (fun actuals ->
        match actuals with
        | x :: more ->
            Print.apply x more
        | _ ->
            Code.apply appearance actuals
      )

    let code =
      id, appearance

  end

  let rot2 f y x =
    f x y

  module Rot2 = struct

    let appearance =
      support "Fun.rot2"

    (* The following is an optional optimization: when [rot2] is applied to at
       least three actual arguments, we can perform beta-reduction on the fly,
       so that the application of [rot2] becomes invisible. This is permitted,
       even though the actual arguments are not necessarily values, because
       [rot2] uses its arguments linearly and because we can assume that at
       most of one the arguments raises an exception. It is definitely a bit
       fragile. *)

    let appearance =
      Code.custom (fun actuals ->
        match actuals with
        | f :: y :: x :: more ->
            (* Someone who is crazy about detail will note that since [f]
               moves from argument position back to head position, it no
               longer needs to be parenthesized; but we have no way of
               removing parentheses. *)
            Print.apply f (x :: y :: more)
        | _ ->
            (* We have fewer than three actual arguments; revert to the normal
               appearance. *)
            Code.apply appearance actuals
      )

    let code =
      rot2, appearance

  end

  let rot3 f z x y =
    f x y z

  module Rot3 = struct

    let appearance =
      support "Fun.rot3"

    let appearance =
      Code.custom (fun actuals ->
        match actuals with
        | f :: z :: x :: y :: more ->
            Print.apply f (x :: y :: z :: more)
        | _ ->
            Code.apply appearance actuals
      )

    let code =
      rot3, appearance

  end

  let curry f x y =
    f (x, y)

  module Curry = struct

    let appearance =
      support "Fun.curry"

    let appearance =
      Code.custom (fun actuals ->
        match actuals with
        | f :: x :: y :: more ->
            Print.apply f (PPrint.OCaml.tuple [ x; y ] :: more)
        | _ ->
            Code.apply appearance actuals
      )

    let code =
      curry, appearance

  end

  let uncurry f (x, y) =
    f x y

  module Uncurry = struct

    let appearance =
      support "Fun.uncurry"

    let code =
      uncurry, appearance

  end

end

(* -------------------------------------------------------------------------- *)

module List = struct

  (* Testing two lists for equality. *)

  let equal eq xs ys =
    List.length xs = List.length ys &&
    List.for_all2 eq xs ys

  (* [List.to_seq] appears in OCaml 4.07. *)

  let rec to_seq xs =
    fun () ->
      match xs with
      | [] ->
          Seq.Nil
      | x :: xs ->
          Seq.Cons (x, to_seq xs)

  module ToSeq = struct

    let appearance =
      support "List.to_seq"

    let code =
      to_seq, appearance

  end

end

(* -------------------------------------------------------------------------- *)

module Exn = struct

  (* Catching all exceptions. *)

  let handle f x =
    try Ok (f x) with
    | Engine.PleaseBackOff -> raise Engine.PleaseBackOff
    | e -> Error e

  module Handle = struct

    let appearance =
      support "Exn.handle"

    let code =
      handle, appearance

  end

end

(* -------------------------------------------------------------------------- *)

module Seq = struct

  include Seq

  (* One-shot functions. *)

  exception ForcedTwice

  let oneshot f =
    let forced = ref false in
    fun x ->
      if !forced then raise ForcedTwice;
      forced := true;
      f x

  (* Affine sequences. *)

  open Seq

  let rec affine xs =
    oneshot (fun () ->
      match xs() with
      | Nil ->
          Nil
      | Cons (x, xs) ->
          Cons (x, affine xs)
    )

  let to_option xs =
    match xs() with
    | Nil ->
        None
    | Cons (x, xs) ->
        Some (x, xs)

  (* The composition [affine . List.to_seq]. *)

  let rec list_to_affine_seq (xs : 'a list) : 'a t =
    oneshot (fun () ->
      match xs with
      | [] ->
          Nil
      | x :: xs ->
          Cons (x, list_to_affine_seq xs)
    )

  module ListToAffineSeq = struct

    let appearance =
      support "Seq.list_to_affine_seq"

    let code =
      list_to_affine_seq, appearance

  end

end

(* -------------------------------------------------------------------------- *)

(* This is a variant of affine sequences where it is possible to test at
   runtime whether a sequence is valid (i.e., can still be forced). *)

module VSeq = struct

  type 'a t = {
    force: unit -> 'a node;
    valid: unit -> bool
  }

  and 'a node =
  | Nil
  | Cons of 'a * 'a t

  let valid xs =
    xs.valid()

  exception ForcedTwice

  let oneshot f =
    let forced = ref false in
    let force x =
      if !forced then raise ForcedTwice;
      forced := true;
      f x
    and valid () =
      not !forced
    in
    { force; valid }

  let rec affine (xs : 'a Seq.t) : 'a t =
    oneshot (fun () ->
      match xs() with
      | Seq.Nil ->
          Nil
      | Seq.Cons (x, xs) ->
          Cons (x, affine xs)
    )

  let to_option xs =
    match xs.force() with
    | Nil ->
        None
    | Cons (x, xs) ->
        Some (x, xs)

  let rec forget (xs : 'a t) : 'a Seq.t =
    fun () ->
      match xs.force() with
      | Nil ->
          Seq.Nil
      | Cons (x, xs) ->
          Seq.Cons (x, forget xs)

end
