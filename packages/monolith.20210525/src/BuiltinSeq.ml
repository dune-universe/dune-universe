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
open Support

(* This file offers ready-made functions that help deal with persistent
   sequences and with affine sequences. *)

(* -------------------------------------------------------------------------- *)

(* Persistent sequences. *)

(* There are two entirely separate aspects that must be dealt with:
   (1) constructing sequences, and (2) deconstructing sequences. *)

(* On the construction side, we generate a sequence by first generating a
   list, then converting this list to a (persistent) sequence. *)

(* On the deconstruction side, we declare the type of sequences as an abstract
   type, equipped with an operation, [to_option]. Nothing prevents Monolith
   from generating scenarios where a sequence is forced twice. This is good:
   we actually test that the sequences produced by the candidate
   implementation are persistent. *)

(* The conversion to an option in [to_option] is required because Monolith
   does not have primitive support for deconstructing a value of type ['a
   Seq.node]. We could likely add such support, but why bother? *)

let declare_seq ?length:(length=Gen.lt 16) element =
  ifpol

    (* The construction side. *)
    begin
      list ~length element
      |> map_outof List.to_seq List.ToSeq.code
    end

    (* The deconstruction side. *)
    begin
      (* Declare an abstract type [seq]. *)
      let seq = declare_abstract_type ~var:"seq" () in
      (* Declare the operation [to_option]. *)
      let spec = seq ^> option (element *** seq) in
      Ops.declare "Sup.Seq.to_option" spec
        Seq.to_option Seq.to_option;
      (* Return the spec [seq]. *)
      seq
    end

(* -------------------------------------------------------------------------- *)

(* Affine sequences. *)

(* The general approach is the same as in the previous case. *)

(* On the construction side, we first generate a list, then convert it
   (directly) to an affine sequence. *)

(* On the deconstruction side, we again declare the type of sequences as an
   abstract type, equipped with an operation [to_option]. We must be careful,
   however, not to apply [to_option] twice to the same sequence. To achieve
   this, we must use an alternative reference implementation, ['a VSeq.t],
   where each suspension keeps track of whether it has been forced already and
   allows retrieving this information at runtime. Furthermore, we hide the
   fact that we use ['a VSeq.t] from the user. The reference implementation
   produces values of type ['a Seq.t]; we map them into the type ['a VSeq.t]
   before deconstructing them. The user does not see this at all; in
   particular, the operation [to_option] is printed as [Seq.to_option], as
   this is the candidate-side implementation. *)

let declare_affine_seq ?length:(length=Gen.lt 16) element =
  ifpol

    (* The construction side. *)
    begin
      list ~length element
      |> map_outof
           Seq.list_to_affine_seq Seq.ListToAffineSeq.code
             (* One may argue that perhaps one could use [List.to_seq]
                on the reference side, as we do not really need to verify
                that the reference implementation consumes its argument
                at most once. *)
    end

    (* The deconstruction side. *)
    begin
      (* Declare the abstract type [seq]. *)
      let seq = declare_abstract_type ~var:"seq" () in
      (* Declare the operation [to_option]. *)
      let spec = VSeq.valid % seq ^> option (element *** seq) in
      Ops.declare "Sup.Seq.to_option"
        spec VSeq.to_option Seq.to_option;
      (* Construct a bridge. *)
      map_into VSeq.affine Fun.Id.code seq
    end
