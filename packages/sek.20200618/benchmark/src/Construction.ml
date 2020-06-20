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

open Settings
open CustomSek
module Cmdline = Shared.Cmdline

(* This code is common to several benchmarks. *)

(* It offers several ways of constructing large ephemeral or persistent
   sequences. *)

(* A deterministic generator of elements. *)

let element : unit -> int =
  let c = ref 0 in
  fun () ->
    let i = !c in
    c := i + 1;
    i

(* Building an ephemeral sequence of size [n] via iterated pushes. *)

let push n : int E.t =
  let s = E.create (-1) in
  for i = 0 to n - 1 do
    E.push front s i
  done;
  s

(* Building an ephemeral sequence of size [n] via a selection of
   randomly chosen [push], [pop], [append], [carve] operations. *)

let rec assemble n : int E.t =
  if n = 0 then
    E.create (-1)
  else if n = 1 then begin
    let s = assemble 0 in
    E.push front s (element());
    s
  end
  else
    match Random.int 10 with
    | 0 | 1 | 2 | 3 ->
        (* Push an element onto a sequence of size [n - 1]. *)
        let s = assemble (n - 1) in
        let side = if Random.bool() then front else back in
        E.push side s (element());
        s
    | 4 | 5 | 6 | 7 ->
        (* Concatenate two sequences of sizes [n1] and [n2]. *)
        let n1 = 1 + Random.int (n - 1) in
        let n2 = n - n1 in
        let s1 = assemble n1
        and s2 = assemble n2 in
        E.append back s1 s2;
        s1
    | 8 ->
        (* Split a sequence of size [n] at an arbitrary position,
           and exchange the two halves. *)
        let s = assemble n in
        let i = 1 + Random.int (n - 1) in
        let s2 = E.carve back s i in
        E.append front s s2;
        s
    | 9 ->
        (* Pop an element off a sequence of size [n + 1]. *)
        (* Almost sure termination. *)
        let s = assemble (n + 1) in
        let side = if Random.bool() then front else back in
        let (_ : int) = E.pop side s in
        s
    | _ ->
        assert false

(* Building a persistent sequence of size [n] by calling [P.make]. This takes
   constant time and yields a sequence where a single schunk is shared. *)

let share n : int P.t =
  P.make (-1) n 0

(* Building an ephemeral sequence via one the construction methods above. *)

let ebuild n : int E.t =
  (* Read the desired construction method. *)
  let c = Cmdline.parse_or_default_string "construction" "push" in
  match c with
  | "push" ->
      push n
  | "assemble" ->
      assemble n
  | "share" ->
      edit (share n)
  | _ ->
      Printf.eprintf "Unknown construction method: \"%s\"\n.%!" c;
      exit 1

(* Building a persistent sequence via one the construction methods above. *)

let pbuild n : int P.t =
  (* Read the desired construction method. *)
  let c = Cmdline.parse_or_default_string "construction" "push" in
  match c with
  | "push" ->
      snapshot_and_clear (push n)
  | "assemble" ->
      snapshot_and_clear (assemble n)
  | "share" ->
      share n
  | _ ->
      Printf.eprintf "Unknown construction method: \"%s\"\n.%!" c;
      exit 1
