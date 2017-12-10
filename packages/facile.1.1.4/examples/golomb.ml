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
(* $Id: golomb.ml,v 1.16 2004/09/07 13:40:47 barnier Exp $ *)

(* Golomb Ruler

  A Golomb ruler is a set of integers (marks) a(1) < ... < a(k) such
that all the differences a(i)-a(j) (i > j) are distinct. Clearly we
may assume a(1)=0. Then a(k) is the length of the Golomb ruler. For a
given number of marks, k, we are interested in finding the shortest
Golomb rulers. Such rulers are called optimal.
*)

open Facile
open Easy

let golomb n =
  (* Tick marks between 0 and 2^n (bound obtained with a (very) greedy
     method) *)
  let n2 = (truncate (2.**float n))
  and dummy = Fd.int 0 in
  let ticks = Fd.array n 0 n2 
  and dists = Array.create ((n*(n-1))/2) dummy in

  (* Constraints *)
  Fd.unify ticks.(0) 0; (* First tick at the start of the ruler *)
  let cpt = ref 0 in
  (* Compute all the distances *)
  for i = 0 to n - 1 do
    (* Ticks are ordered *)
    if i < n-1 then Cstr.post (fd2e ticks.(i+1) >~ fd2e ticks.(i));
    for j = i+1 to n - 1 do
      dists.(!cpt) <- Arith.e2fd (fd2e ticks.(j) -~ fd2e ticks.(i));
      Cstr.post (fd2e dists.(!cpt) >~ i2e 0);
      incr cpt
    done
  done;
  (* All the distances are distinct *)
  (***) Cstr.post (Alldiff.cstr ~algo:(Alldiff.Bin_matching Var.Fd.on_subst) dists);
  (***
  for i = 0 to Array.length dists - 1 do
    for j = i+1 to Array.length dists - 1 do
      Cstr.post (fd2e dists.(i) <>~ fd2e dists.(j))
    done
  done;
***)
    
 
  (* Breaking the symmetry *)
  Cstr.post (fd2e dists.(!cpt - 1) >~ fd2e dists.(0));

  (* Search Goal *)
  let goal = Goals.Array.labeling ticks in

  (* Search fot the optimal solution: minimal last tick *)
  let bt = ref 0 in
  ignore
    (Goals.solve
       ~control:(fun b -> bt := b)
       (Goals.minimize
	  goal
	  ticks.(n-1)
	  (fun _cost ->
       	    Printf.printf "Found better: ";
	    Array.iter (fun t -> Printf.printf "%a " Fd.fprint t) ticks;
	    Printf.printf "(%d backtracks)" !bt;
	    print_newline ())));
  Printf.printf "%d backtracks\n" !bt

let _ =
  Gc.set ({(Gc.get ()) with Gc.space_overhead = 100});
  if Array.length Sys.argv < 2 then
    prerr_endline "Usage: golomb <size>"
  else
    golomb (int_of_string Sys.argv.(1));;

(*
	Reveil sur inst	Reveil sur ref	Alldiff basique		n^2 <>
	CPU	Btks	CPU	Btks	CPU	Backtracks	CPU	Backtracks
7	0.09	236	0.09	181	0.1	357		0.3	357
8	0.84	1669	0.72	1131	1.1	2740		4.4	2740
9	7.56	9919	5.8	5915	12.32	19452		56	19452
10	69.8	62474	50.6	34254	121	140752		708	140752
*)
