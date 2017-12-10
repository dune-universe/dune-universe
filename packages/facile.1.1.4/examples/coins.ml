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
(* $Id: coins.ml,v 1.13 2004/09/07 09:24:43 barnier Exp $ *)

(*
   Which coins do you need to give back change for any amount between
   0 and [max], using coins from [values].
*)

open Facile
open Easy

let coins values max =
  let domains = Array.map (fun v -> Domain.interval 0 (max / v)) values in
  let gen_vars () = Array.map (fun d -> Fd.create d) domains in

  (* The solution *)
  let nb_min_coins = gen_vars () in

  let mat =
    Array.init max
      (fun i ->
	(* coins needed to give back i *)
	let nb_coins = gen_vars () in
	Cstr.post (Arith.scalprod_fd values nb_coins =~ i2e i);
	for j = 0 to Array.length nb_coins - 1 do
	  let nbpj = nb_coins.(j)
	  and nbmpj = nb_min_coins.(j) in
	  Cstr.post (fd2e nbpj <=~ fd2e nbmpj)
	done;
	nb_coins) in

  (* Cost: nb of coins *)
  let cost = Arith.e2fd (Arith.sum_fd nb_min_coins) in
  let cost = Fd.interval 0 max in
  Cstr.post (fd2e cost =~ Arith.sum_fd nb_min_coins);

  (* Search goal *)
  let goal =
    Goals.Array.forall Goals.Array.labeling mat
      &&~ Goals.Array.labeling nb_min_coins in

  (* Searching for the best solution *)
  let best = ref [||] in
  ignore
    (Goals.solve
       (Goals.minimize goal cost
	  (fun c ->
	    Printf.printf "%d found\n" c; flush stdout;
	    best := Array.map Fd.int_value nb_min_coins)));
  match !best with
    [||] -> prerr_endline "No solution"
  | sol -> Array.iter (fun x -> Printf.printf "%d " x) sol; print_newline ();; 

let _ =
  coins [|1;2;5;10;20|] 100;;
