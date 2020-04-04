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

(* The abstract syntax of test programs. *)

(* Test programs contain variables that denote sequences. A variable is bound
   by [LetCreate], [LetCopy], etc., and is used by [Push], [Pop], etc.
   Variables are encoded as de Bruijn indices. *)

(* We have variables of several distinct types, such as ephemeral sequences
   and persistent sequences. These variables inhabit a single namespace. *)

(* When adding a new data constructor to this type, do not forget to update
   the generator in DSLGenerator. The OCaml type-checker does not detect
   non-exhaustive generators! *)

type instruction =

  | Nop

  (* Operations on ephemeral sequences. *)

  | Push of side * eseqvar * element
      (* push side x e *)
  | Pop of side * element option observation * eseqvar
      (* assert (e = pop side x) *)
  | Clear of eseqvar
      (* clear x *)
  | Assign of eseqvar * eseqvar
      (* assign x1 x2 *)
  | LetCopy of eseqvar
      (* let y = copy x *)
  | Append of side * eseqvar * eseqvar
      (* append side x1 x2 *)
  | LetCarve of side * eseqvar * int
      (* let y = carve side x k *)
  | Set of eseqvar * int * element
      (* set x i e *)

  (* Operations on persistent sequences. *)

  | LetPPush of side * pseqvar * element
      (* let y = push e x *)
  | LetPPop of side * element option observation * pseqvar
      (* let o, y = pop x in assert (e = o) *)
  | LetPSet of pseqvar * int * element
      (* let x = set x i e *)

  (* Operations that exist both on ephemeral and persistent sequences
     (and have the same type in both cases). *)

  | LetCreate of kind * element
      (* let x = create e *)
  | LetMake of kind * element * int * element
      (* let x = make d n e *)
  | LetInit of kind * element * int
      (* let x = init d n (delay (ref 0) (fun i -> i)) *)
  | Length of int observation * seqvar
      (* assert (i = length x) *)
  | IsEmpty of bool observation * seqvar
      (* assert (b = is_empty x) *)
  | Peek of side * element option observation * seqvar
      (* assert (e = peek x) *)
  | Get of element observation * seqvar * int
      (* let o = get x k in assert (e = o) *)
  | LetConcat of kind * seqvar * seqvar
      (* let z = concat x y *)
  | LetSplit of kind * seqvar * int
      (* let y1, y2 = split x k *)
  | Iteri of (int * int) list observation * direction * seqvar
      (* let ixs = ref [] in
         iteri direction (fun i x -> ixs := (i, x) :: !ixs) x;
         assert (... = List.rev !ixs) *)
  | ToArray of int array observation * seqvar
      (* assert (a = to_array x) *)
  | LetOfArray of kind * element * int
      (* let x = of_array e (Array.init n (fun x -> x)) *)
  | LetOfArraySegment of kind * element * int * int * int
      (* let x = of_array_segment e (Array.init n (fun x -> x)) h k *)

  (* Conversions between ephemeral and persistent sequences. *)

  | LetSnapshot of eseqvar
      (* let p = snapshot x *)
  | LetEdit of pseqvar
      (* let x = edit p *)
  | LetSnapshotAndClear of eseqvar
      (* let p = destructive_snapshot x *)

and instructions =
  instruction list

(* An ephemeral vs. persistent choice appears in some instructions. *)

and kind =
  | E
  | P

(* The front and back sides. *)

and side =
  | Front
  | Back

(* Directions. *)

and direction =
  | Forward
  | Backward

(* We use elements of type [int]. *)

and element =
  int

(* Variables are encoded as de Bruijn indices. *)

and eseqvar =
  var

and pseqvar =
  var

and seqvar =
  var

and var =
  int

(* A [Pop] instruction is optionally annotated with an observation,
   which itself is an optional element (as [pop] can return either
   an element or nothing). When such an observation is present, it
   must have been produced by the reference implementation, and it
   represents an expected result. *)

(* Other instructions can be annotated with observations of other
   types, e.g., [to_list] is annotated with an observation of a
   list of elements. *)

and 'a observation =
  'a option
