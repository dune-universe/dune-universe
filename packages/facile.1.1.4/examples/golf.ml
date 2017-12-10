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
(* $Id: golf.ml,v 1.21 2003/07/18 14:55:43 brisset Exp $ *)

(*
  A Golf Tournament (from http://www.icparc.ic.ac.uk/~cg6/conjunto.html)

  There are 32 golfers, who play individually but in groups of 4,
  called foursomes. The tournament is organized in weeks.  Each week
  a new set of foursomes has to be computed such that each person only
  golfs with the same person once. So if two golfers have played each
  other in any previous week they should not play each other in the
  coming weeks. The question is: "how many weeks can we ensure this
  before players start to play each other a second time ?"

  The formulation is generalized to any number of golfers, groups and weeks.
*)

open Facile
open Easy

(* Two modes using the Global Cardinality Constraint or the Sort constraint *)
type mode = Gcc | Sort
let mode_of = function
    "gcc" -> Gcc | "sort" -> Sort | _ -> failwith "Unknown mode";;

let go nb_groups size_group nb_weeks mode =
  let nb_golfers = nb_groups * size_group in

  (* An array of nb_weeks*nb_golfers decision variables to choose the group
     (in 0..nb_groups-1) of every golfer every week *)
  let vars =
    Array.init
      nb_weeks
      (fun _ -> Fd.array nb_golfers 0 (nb_groups-1)) in

  (* Constraints *)
  (* For each week, exactly size_group golfers in each group: *)
  begin
    match mode with
      Gcc ->
	(* Using a Global Cardinality Constraint *)
  	let cards_values = Array.init nb_groups (fun i -> (Fd.int size_group, i)) in
  	for w = 0 to nb_weeks - 1 do
	  Cstr.post (Gcc.cstr vars.(w) cards_values)
  	done
    | Sort ->
	(* Using a Sorting constraint: For each week the sorted array of
	   groups is equal to [|0;0;0;0;1;1;1;1;2;2;2;2;....|] *)
	let sorted =
	  Array.init nb_golfers (fun i -> Fd.int (i / size_group)) in
	for j = 0 to nb_weeks - 1 do
	  Cstr.post (Sorting.cstr vars.(j) sorted)
  	done
  end;

  (* Two golfers do not play in the same group more than once *)
  for g1 = 0 to nb_golfers - 1 do (* for each pair of golfers *)
    for g2 = g1+1 to nb_golfers - 1 do
      let g1_with_g2 =
	Array.init nb_weeks (fun w -> Arith.e2fd (fd2e vars.(w).(g1) =~~ fd2e vars.(w).(g2))) in
      Cstr.post (Arith.sum_fd g1_with_g2 <=~ i2e 1)
    done
  done;

  (* Breaking the symmetries
     0 always in the first group, 1 in a group less than 1, ...
     First week (0) a priori chosen
     *)
  for w = 0 to nb_weeks - 1 do
    for g = 0 to nb_groups - 1 do
      Cstr.post (fd2e vars.(w).(g) <=~ i2e g)
    done
  done;
  for g = 0 to nb_golfers - 1 do 
    Cstr.post (fd2e vars.(0).(g) =~ i2e (g / size_group))
  done;

  (* Seach goal: Choose the groups for the first golfer, then for second
     one, ... *)
  let goal =
    Goals.forto 0 (nb_golfers-1)
      (fun g -> Goals.forto 0 (nb_weeks-1)
	  (fun w -> Goals.indomain vars.(w).(g))) in

  (* Solving *)
  let nb_backtracks = ref 0
  and start = Sys.time () in
  if Goals.solve ~control:(fun n -> nb_backtracks := n) goal then begin
    Printf.printf "Found a solution in %.2fs\n" (Sys.time () -. start);
    for w = 0 to nb_weeks - 1 do
      for g = 0 to nb_groups do
      	for p = 0 to nb_golfers - 1 do
	  if Fd.int_value vars.(w).(p) = g then Printf.printf "%2d " p
	done;
	print_string " ";
      done;
      print_newline ()
    done
  end else
    prerr_endline "No solution";
  Printf.printf "with %d fails\n" !nb_backtracks;;

let _ =
  if Array.length Sys.argv < 5 then
    prerr_endline "Usage: golf <nb groups> <size group> <nb_weeks> <mode> (gcc or sort)"
  else
    let nb_groups = int_of_string Sys.argv.(1)
    and size_group = int_of_string Sys.argv.(2)
    and nb_weeks = int_of_string Sys.argv.(3)
    and mode = mode_of Sys.argv.(4) in
    go nb_groups size_group nb_weeks mode;;
  
