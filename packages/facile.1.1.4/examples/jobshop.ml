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
open Facile
open Easy

module T = Scheduling.Task

let read_mt file =
  let inbuf = Scanf.Scanning.from_file file in
  let (nb_job, nb_task) =
    Scanf.bscanf inbuf "%d %d " (fun a b -> (a, b)) in
  let duration = Array.make_matrix nb_job nb_task 0 
  and num_mach = Array.make_matrix nb_job nb_task 0 in
  
  for i=0 to nb_job-1 do
    for j=0 to nb_task-1 do
      Scanf.bscanf inbuf "%d %d "
    	(fun a b -> duration.(i).(j) <- a; num_mach.(i).(j) <- b - 1)
    done done;
  (duration, num_mach)

let js file = 
  let cpu_start = Sys.time () in
  let (duration, num_mach) = read_mt file in
  let nb_job = Array.length duration 
  and nb_task = Array.length duration.(0) in
  let horizon = Array.fold_left (Array.fold_left (+)) 0 duration in
  let machs = Array.create nb_task []
  and sums = Array.create nb_task 0 in
  let a =
    Array.mapi
      (fun i jobi ->
	Array.mapi
	  (fun j pij ->
	    let s = Fd.interval 0 (horizon - pij) in
	    let name = Printf.sprintf "%d %d" (i+1) (j+1) in
	    let a = T.create ~name s pij in
	    let m = num_mach.(i).(j) in
	    sums.(m) <- sums.(m) + pij;
	    machs.(m) <- a :: machs.(m);
	    a)
	  jobi)
      duration in

  let jobs_ends =
    Array.init nb_job
      (fun i -> Arith.e2fd (T.end_time a.(i).(nb_task-1))) in
  let makespan = FdArray.max jobs_ends in

  let schedules = Array.map (fun m -> Scheduling.create m) machs in

  (* Machines constraints *)
  Array.iter (fun s -> Cstr.post (Scheduling.edge_finding s)) schedules;
  (* Redondant constraint : fewer backtracks but higher cpu *)
  (* Not enough to find alone mt10 optimal solution *)
  (* Array.iter (fun s -> Cstr.post (Scheduling.disjunctive s)) schedules;*)

  (* precedence *)
  for i = 0 to nb_job-1 do
    for j = 1 to nb_task-1 do
      Cstr.post (T.after a.(i).(j) a.(i).(j-1))
    done done;

  (* Goal: ranking ressource by ressource, starting with the most critical *)
  let max_deadline i =
    List.fold_left (fun r a -> max r (T.deadline a)) min_int machs.(i) in

  let min_release_date i =
    List.fold_left (fun r a -> min r (T.release_date a)) max_int machs.(i) in

  let ranked = Array.init nb_task (fun _ -> Stak.ref false) in
  let most_critical schedules =
    let best = ref (-1) and slack_best = ref max_int in
    for i = 0 to nb_task - 1 do
      if not (Stak.get ranked.(i)) then
	let mi = max_deadline i - min_release_date i - sums.(i) in
      	if mi < !slack_best then begin
	  best := i;
	  slack_best := mi
      	end
    done;
    if !best >= 0 then begin
      Stak.set ranked.(!best) true;
      !best
    end else
      raise Not_found in

  let goal =
    Goals.Array.forall ~select:most_critical Scheduling.Goals.rank schedules
      &&~
    Goals.Array.forall Goals.Array.labeling (Array.map (Array.map T.start) a) in

  Printf.printf "Constraints set: %.2fs\n" (Sys.time() -. cpu_start);

  let control b = Printf.printf "\b\b\b\b\b\b%d" b; flush stdout in
  let schedules_sol = ref None in
  let schedules2triples sched =
    Array.map
      (fun s ->
	List.map
	  (fun a ->
	    (Fd.elt_value (T.start a),
	     T.processing_time a,
	     Arith.eval (T.end_time a)))
	  (Scheduling.tasks s)) sched in
  let solution cost =
    Printf.printf " cost=%d cpu=%.2fs\n" cost (Sys.time () -. cpu_start);
    flush stdout;
    schedules_sol := Some (schedules2triples schedules) in

  ignore (Goals.solve ~control (Goals.minimize goal makespan solution));

  let total = Sys.time () -. cpu_start in
  begin match !schedules_sol with
    Some sol ->
      Printf.printf "\n";
      Array.iter
	(fun s ->
	  List.iter (fun (s, p, e) -> Printf.printf "%d -- %d --> %d\n" s p e) s;
	  Printf.printf "\n") sol
  | None -> Printf.printf "No solution\n" end;
  Printf.printf " cpu=%.2fs\n" total

let _ =
  let data_file = ref "mt10.dat" in
  Arg.parse [] (fun s -> data_file := s) "";
  (* if you have a lot of RAM *)
(*  Gc.set ({(Gc.get ()) with Gc.space_overhead = 500});*)
  js !data_file
