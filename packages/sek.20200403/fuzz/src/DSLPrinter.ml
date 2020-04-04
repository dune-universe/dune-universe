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

open Misc
open Printf
open Printing
open DSLSyntax
let pp_element = pp_int

(* We print test programs in the form of valid OCaml code, so tests can easily be
   reproduced in the OCaml toplevel. *)

(* A static environment maps a variable to a type. *)

type typ =
  | TESeq
  | TPSeq

type senv =
  typ list

let empty =
  []

let lookup : senv -> var -> typ =
  lookup

let nvars senv =
  List.length senv

(* [var senv x] prints the variable [x]. *)

let var senv x =
  (* Convert de Bruijn index to de Bruijn level, then apply a fixed mapping of
     de Bruijn levels to strings. We use a different mapping at each type. For
     ephemeral sequences, we use the names 'a' to 'z' at first, then switch to
     a numeric scheme. For persistent sequences, we use the names 'aa' to 'zz'
     at first, then switch to a numeric scheme. *)
  let k, prefix =
    match lookup senv x with
    | TESeq ->
        1, "e"
    | TPSeq ->
        2, "p"
  in
  let x = nvars senv - x - 1 in
  assert (0 <= x);
  if x < 26 then
    let c = Char.chr (Char.code 'a' + x) in
    String.make k c
  else
    Printf.sprintf "%s%d" prefix x

(* Finding out the kind (ephemeral or persistent) of a variable. *)

let kind_of_typ typ =
  match typ with
  | TESeq -> E
  | TPSeq -> P

let kind senv x =
  kind_of_typ (lookup senv x)

let seqtyp kind =
  match kind with
  | E -> TESeq
  | P -> TPSeq

let print_kind = function
  | E -> "E"
  | P -> "P"

(* [ebind senv] introduces one more ephemeral variable.
   [pbind senv] introduces one more persistent variable. *)

let bind senv typ =
  typ :: senv

let ebind senv =
  bind senv TESeq

let pbind senv =
  bind senv TPSeq

let seqbind senv kind =
  bind senv (seqtyp kind)

(* [iter senv f] applies the function [f] to all variables in scope,
   together with their type. *)

let iter (senv : senv) (f : var * typ -> unit) : unit =
  index 0 senv |> List.iter f

(* [print_pc f pc] prints the current instruction counter as an OCaml comment. *)

let print_pc f pc =
  fprintf f "(* @%02d *) " pc

let print_no_pc f =
  fprintf f "          "

(* [eoi f] ends an instruction with a semicolon, newline and indentation. *)

let eoi f =
  fprintf f ";;\n"

(* [print_observation_check f oo] emits code to check that the variable
   [observed] (which by convention holds the result of the candidate
   implementation) matches the expected observation [oo]. The observation
   [oo] is optional; no code is emitted if it is absent. *)

let print_observation_check f pp (oo : 'a observation) =
  match oo with
  | None ->
      ()
  | Some obs ->
      fprintf f ";; assert (observed = %a)"
        pp obs

(* [print_side] prints a side. *)

let print_side side =
  match side with
  | Front -> "front"
  | Back -> "back"

(* [print_direction] prints a direction. *)

let print_direction direction =
  match direction with
  | Forward -> "forward"
  | Backward -> "backward"

(* Emitting a well-formedness check for all variables in scope. *)

let print_check_wf senv f (x, typ) =
  print_no_pc f;
  fprintf f "%s.check %s" (print_kind (kind_of_typ typ)) (var senv x);
  eoi f

let print_wf senv f =
  iter senv (print_check_wf senv f)

(* [print senv pc] is an instruction printer. [pc] is the current instruction
   counter. *)

let rec print (senv : senv) (pc : int) (f : out_channel) (pwf : bool) (is : instructions) =
  match is with
  | [] ->
      ()
  | i :: is ->
      (* Now, print the instruction itself. *)
      print_pc f pc;
      let senv = print1 senv i f in
      eoi f;
      (* After every instruction, our interpreter performs well-formedness
         checks. We print them only after the last instruction and only if
         [pwf] is set. This printing is not perfect, as we print all [check]
         instructions, possibly further than the one that fails. *)
      if pwf && is = [] then print_wf senv f;
      let pc = pc + 1 in
      print senv pc f pwf is

(* [print1 senv i f] prints the instruction [i] and returns an updated
   static environment that is in force after this instruction. *)

and print1 (senv : senv) (i : instruction) (f : out_channel) : senv =
  match i with
  | Nop ->
      fprintf f "()";
      senv
  | Push (side, x, e) ->
      fprintf f "E.push %s %s %d" (print_side side) (var senv x) e;
      senv
  | Pop (side, oo, x) ->
      fprintf f "let observed = E.pop_opt %s %s" (print_side side) (var senv x);
      print_observation_check f (pp_option pp_element) oo;
      senv
  | Clear x ->
      fprintf f "E.clear %s" (var senv x);
      senv
  | Assign (x1, x2) ->
      fprintf f "E.assign %s %s" (var senv x1) (var senv x2);
      senv
  | LetCopy x ->
      let senv' = ebind senv in
      fprintf f "let %s = E.copy %s" (var senv' 0) (var senv x);
      senv'
  | Set (x, k, e) ->
      fprintf f "E.set %s %d %d" (var senv x) k e;
      senv
  | Append (side, x1, x2) ->
      fprintf f "E.append %s %s %s" (print_side side) (var senv x1) (var senv x2);
      senv
  | LetCarve (side, x, k) ->
      let senv' = ebind senv in
      fprintf f
        "let %s = E.carve %s %s %d"
        (var senv' 0) (print_side side) (var senv x) k;
      senv'
  | LetCreate (kind, e) ->
      let senv' = seqbind senv kind in
      fprintf f "let %s = %s.create (%d)" (var senv' 0) (print_kind kind) e;
      senv'
  | LetMake (kind, d, n, e) ->
      let senv' = seqbind senv kind in
      fprintf f "let %s = %s.make (%d) %d %d" (var senv' 0) (print_kind kind) d n e;
      senv'
  | LetInit (kind, d, n) ->
      let senv' = seqbind senv kind in
      fprintf f "let %s = \
                   let delay r f y = let x = !r in r := f y; x in \
                   %s.init (%d) %d (delay (ref 0) (fun i -> i))"
        (var senv' 0) (print_kind kind) d n;
      senv'
  | LetOfArray (kind, e, n) ->
      let senv' = seqbind senv kind in
      fprintf f "let %s = %s.of_array (%d) (Array.init %d (fun x -> x))"
        (var senv' 0) (print_kind kind) e n;
      senv'
  | LetOfArraySegment (kind, e, n, h, k) ->
      let senv' = seqbind senv kind in
      fprintf f "let %s = %s.of_array_segment (%d) (Array.init %d (fun x -> x)) %d %d"
        (var senv' 0) (print_kind kind) e n h k;
      senv'
  | LetPPush (side, x, e) ->
      let senv' = pbind senv in
      fprintf f "let %s = P.push %s %s %d" (var senv' 0) (print_side side) (var senv x) e;
      senv'
  | LetPPop (side, oo, x) ->
      let senv' = pbind senv in
      fprintf f "let observed, %s = P.pop %s_opt %s" (var senv' 0) (print_side side) (var senv x);
      print_observation_check f (pp_option pp_element) oo;
      senv'
  | LetConcat (kind, x1, x2) ->
      let senv' = seqbind senv kind in
      fprintf f "let %s = %s.concat %s %s"
        (var senv' 0) (print_kind kind) (var senv x1) (var senv x2);
      senv'
  | LetSplit (kind, x, k) ->
      let senv' = seqbind (seqbind senv kind) kind in
      fprintf f "let %s, %s = %s.split %s %d"
        (var senv' 0) (print_kind kind) (var senv' 1) (var senv x) k;
      senv'
  | LetPSet (x, k, e) ->
      let senv' = pbind senv in
      fprintf f "let %s = P.set %s %d %d" (var senv' 0) (var senv x) k e;
      senv'
  | Length (oo, x) ->
      fprintf f "let observed = %s.length %s"
        (print_kind (kind senv x)) (var senv x);
      print_observation_check f pp_int oo;
      senv
  | IsEmpty (oo, x) ->
      fprintf f "let observed = %s.is_empty %s"
        (print_kind (kind senv x)) (var senv x);
      print_observation_check f pp_bool oo;
      senv
  | Peek (side, oo, x) ->
      fprintf f "let observed = %s.peek_opt %s %s"
        (print_kind (kind senv x)) (print_side side) (var senv x);
      print_observation_check f (pp_option pp_element) oo;
      senv
  | Get (o, x, k) ->
      fprintf f "let observed = %s.get %s %d"
        (print_kind (kind senv x)) (var senv x) k;
      print_observation_check f pp_element o;
      senv
  | ToArray (oo, x) ->
      fprintf f "let observed = %s.to_array %s"
        (print_kind (kind senv x)) (var senv x);
      print_observation_check f (pp_array pp_element) oo;
      senv
  | Iteri (oo, direction, x) ->
      fprintf f "let observed = (let ixs = ref [] in %s.iteri %s (fun i x -> ixs := (i, x) :: !ixs) %s; List.rev !ixs)"
        (print_kind (kind senv x)) (print_direction direction) (var senv x);
      print_observation_check f (pp_list (pp_pair pp_int pp_int)) oo;
      senv
  | LetSnapshot x ->
      let senv' = pbind senv in
      fprintf f "let %s = snapshot %s" (var senv' 0) (var senv x);
      senv'
  | LetEdit p ->
      let senv' = ebind senv in
      fprintf f "let %s = edit %s" (var senv' 0) (var senv p);
      senv'
  | LetSnapshotAndClear x ->
      let senv' = pbind senv in
      fprintf f "let %s = snapshot_and_clear %s" (var senv' 0) (var senv x);
      senv'

let print =
  print empty 0
