(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: magic.ml,v 1.9 2001/06/01 14:13:09 barnier Exp $ *)

(*
  Magic Sequence

  A magic sequence is a sequence of N values (x0, x1, , xN-1) such
that 0 will appear in the sequence x0 times, 1 will appear x1
times,..., and N-1 will appear in the sequence xN-1 times. For example,
for N=3, the following sequence is a solution: (1, 2, 1, 0). That is,
0 is present once, 1 is present twice, 2 is present once, and 3 is not
present.
*)

open Facile
open Easy

let magic n =
  (* n variables *)
  let x = Fd.array n 0 (n-1) in

  (* Constraint: cardinality constraint with x as variables and cardinals *)
  let card_vals = Array.mapi (fun i x -> (x, i)) x in
  Cstr.post (Gcc.cstr ~level:Gcc.Medium x card_vals);

  (* Redundant constraints *)
  let vals = Array.init n (fun i -> i) in
  Cstr.post (Arith.scalprod_fd vals x =~ i2e n);

  (* Search goal: first fail with min domain size *)
  let min_size = 
    Goals.Array.choose_index (fun a1 a2 -> Var.Attr.size a1 < Var.Attr.size a2) in
  let goal = Goals.Array.forall ~select:min_size Goals.indomain x in

  (* Search *)
  if Goals.solve goal then begin
    Array.iter (fun v -> Printf.printf "%a " Fd.fprint v) x; print_newline ()
  end
  else
    prerr_endline "No solution";;

let _ =
  if Array.length Sys.argv < 2 then prerr_endline "Usage: magic <size>"
  else magic (int_of_string Sys.argv.(1));;
