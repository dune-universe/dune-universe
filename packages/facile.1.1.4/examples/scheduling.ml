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
(* $Id: scheduling.ml,v 1.4 2004/08/12 15:31:52 barnier Exp $ *)

open Facile
open Easy

module Task = struct
  type t = {
      start : Fd.t;
      end_time : Arith.t;
      processing_time : int;
      name : string ;
      id : int
    }

  let gen_int = Misc.gen_int_fun ()

  let create ?(name = "") s p = {
    start = s;
    end_time = fd2e s +~ i2e p;
    processing_time = p;
    name = name;
    id = gen_int ()
  }
  let id a = a.id
  let name a = a.name
  let start a = a.start
  let end_time a = a.end_time
  let release_date a = Fd.min a.start
  let processing_time a = a.processing_time
  let deadline a = Fd.max a.start + a.processing_time

  let update_release_date a x =
    let s = a.start in
    match Fd.value s with
      Unk attra -> Fd.refine s (Domain.remove_low x (Var.Attr.dom attra))
    | Val v -> if x > v then Stak.fail "Scheduling.Task.update_release_date"
  let update_deadline a x =
    let s = a.start in
    match Fd.value s with
      Unk attra ->
	Fd.refine s
	  (Domain.remove_up (x - a.processing_time) (Var.Attr.dom attra))
    | Val v ->
	if x < v + a.processing_time then
	  Stak.fail "Scheduling.Task.update_deadline"

  let end_time_bounds a = Arith.min_max_of_expr a.end_time
  let fprint_end_time c a =
    let (mi, ma) = end_time_bounds a in
    if mi = ma then Printf.fprintf c "%d" mi
    else Printf.fprintf c "[%d-%d]" mi ma
  let before a1 a2 = a1.end_time <=~ fd2e a2.start
  let after a1 a2 = before a2 a1
  let fprint c a =
    Printf.fprintf c "%a -- %d --> %a\n"
      Fd.fprint a.start a.processing_time fprint_end_time a
end

type t = Task.t array

let fprint f s = Array.iter (Task.fprint f) s  
let create = Array.of_list
let tasks = Array.to_list
let iter = Array.iter
let number_of_tasks = Array.length


(* Edge Finding *)

(* From Constraint-Based Scheduling, page 24 *)
(*
   [cc] maximal minimal end time
   [c.(i)] maximal minimal end time of A_j that come after A-i
   [pp] sum of processing times
*)
module T = Task
let compare_release_dates a a' = compare (T.release_date a) (T.release_date a')

let update_r a =
  let n = Array.length a in
  (* First sort {A_1,...,A_n} ascending release dates *)
  Array.sort compare_release_dates a;
  (* Check first if it is globally consistent *)
  try
    for i = 1 to n - 1 do
      if T.release_date a.(i) < T.deadline a.(i-1) then raise Exit done;
    true
  with Exit -> (* Copy release_dates in [r] and do updates directly in [a] *)
    let r = Array.map T.release_date a in (* 1..3 *)
    let c = Array.create n 0 in 
    for k = 0 to n-1 do (* 4..29 *)
      let pp = ref 0
      and cc = ref min_int in
      let dk = T.deadline a.(k) in
      for i = n-1 downto 0 do (* 6..15 *)
	let di = T.deadline a.(i)
	and pi = Task.processing_time a.(i) in
	if di <= dk then begin
	  pp := !pp + pi;
	  cc := max !cc (r.(i) + !pp);
	  if !cc > dk then Stak.fail "Scheduling.update_r" end;
	c.(i) <- !cc done;
      let hh = ref min_int in
      for i = 0 to n-1 do (* 16..28 *)
	let di = T.deadline a.(i)
	and pi = T.processing_time a.(i) in
	if di <= dk then begin (* 17..19 *)
	  hh := max !hh (r.(i) + !pp);
	  pp := !pp - pi end
	else begin (* 20..26 *)
	  if r.(i) + !pp + pi > dk then T.update_release_date a.(i) c.(i);
	  if !hh + pi > dk then T.update_release_date a.(i) !cc end
      done
    done;
    false (* Check is done at the beginning of update *)

let compare_deadlines a a' = compare (T.deadline a) (T.deadline a')

let update_d a =
  let n = Array.length a in
  (* First sort {A_1,...,A_n} asscending deadline dates *)
  Array.sort compare_deadlines a;
  (* Check done ONLY in update_r *)
  (* Copy deadlines in [d] and do updates directly in [a] *)
  let d = Array.map T.deadline a in (* 1..3 *)
  let c = Array.create n 0 in 
  for k = n-1 downto 0 do (* 4..29 *)
    let pp = ref 0
    and cc = ref max_int in
    let rk = T.release_date a.(k) in
    for i = 0 to n-1  do (* 6..15 *)
      let ri = T.release_date a.(i)
      and pi = Task.processing_time a.(i)
      in
      if ri >= rk then begin
	pp := !pp + pi;
	cc := min !cc (d.(i) - !pp);
	if !cc < rk then Stak.fail "Scheduling.update_d" end;
      c.(i) <- !cc done;
    let hh = ref max_int in
    for i = n-1 downto 0 do (* 16..28 *)
      let ri = T.release_date a.(i)
      and pi = T.processing_time a.(i)
      in
      if ri >= rk then begin (* 17..19 *)
	hh := min !hh (d.(i) - !pp);
	pp := !pp - pi end
      else begin (* 20..26 *)
	if d.(i) - (!pp + pi) < rk then T.update_deadline a.(i) c.(i);
	if !hh - pi < rk then T.update_deadline a.(i) !cc end
    done
  done;
  false (* Check is done at the beginning of update *)

let edge_finding = fun a ->
  let update _ = update_r a || update_d a
  and delay ct =
    Array.iter
      (fun a ->
	Fd.delay [Var.Fd.on_min; Var.Fd.on_max] (Task.start a) ct) a
  and priority = Cstr.later
  and name = "Scheduling.edge_finding" in
  Cstr.create ~priority ~name update delay

let disjunctive2 a1 a2 =
  let update _ =
    T.release_date a2 >= T.deadline a1 ||
    let p1p2 = T.processing_time a1 + T.processing_time a2 in
    if T.release_date a2 + p1p2 > T.deadline a1 then begin
      (* a2 cannot be before a1 *)
      Cstr.post (fd2e (T.start a2) >=~ T.end_time a1);
      true end
    else false
  and delay ct =
    Fd.delay [Var.Fd.on_max] (Task.start a1) ct;
    Fd.delay [Var.Fd.on_min] (Task.start a2) ct
  and priority = Cstr.later
  and name = "Scheduling.disjunctive2" in
  Cstr.create ~priority ~name update delay

let disjunctive a =
  let n = Array.length a and c = ref Cstr.one in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i <> j then c := Reify.(&&~~) !c (disjunctive2 a.(i) a.(j))
    done done;
  !c


module Goals = struct
  let rank s =
    let n = Array.length s
    and s = Array.copy s in (* We will swap elements *)
    let swap i j = let tmp = s.(i) in s.(i) <- s.(j); s.(j) <- tmp
    and arg_smallest_release_date i =
      let value j =
	(T.release_date s.(j),
	 T.deadline s.(j) - T.processing_time s.(j),
	 T.id s.(j)) in
      let best = ref i in
      for j = i+1 to n-1 do if value j < value !best then best := j done;
      !best
    and is_first i () =
      let endsi = Task.end_time s.(i) in
      for j = i + 1 to n - 1 do (* Others start after *)
	Cstr.post (fd2e (Task.start s.(j)) >=~ endsi) done
    and is_not_first i () =
      if i = n-1 then Stak.fail "Scheduling.Goals.rank.is_not_first";
      let minimal_end_time_of_another =
	FdArray.min
	  (Array.map
	     (fun a -> Arith.e2fd (T.end_time a))
	     (Array.sub s (i+1) (n-i-1))) in
      (***Printf.printf "%a >= %a\n" Fd.fprint (Task.start s.(i)) Fd.fprint minimal_end_time_of_another; flush stdout; ***)
      Cstr.post
	(fd2e (Task.start s.(i)) >=~ fd2e minimal_end_time_of_another) in
(***
    let rec rank i =
      Goals.create
      	(fun i ->
	  (***Printf.printf "rank %d: \n" i;
	  fprint stdout s; ***)
	  if i < n then
	    let j = arg_smallest_release_date i in
	    (*** Printf.printf "i=%d j=%d\n" i j; ***)
	    swap i j;
	    Goals.(||~)
	      (Goals.(&&~) (Goals.atomic (is_first i)) (rank (i+1)))
	      (Goals.(&&~) (Goals.atomic (is_not_first i)) (rank i))
	  else begin
            (***fprint stdout s; ***)
	    Goals.success end)
	i in
    rank 0
***)

    let swap_bt i j () =
      if i <> j then begin
	let tmp = s.(i) in
	s.(i) <- s.(j); s.(j) <- tmp;
	Stak.trail (fun () -> swap i j) end in
    (*
    let another_first i j () =
      let latest_start_j = T.deadline s.(j) - T.processing_time s.(j) in
      try
	for i = i to n - 1 do
	  if i <> j && T.release_date s.(i) + T.processing_time s.(i) <= latest_start_j then raise Exit done;
	Stak.fail "Scheduling.Goals.another_first"
      with Exit -> () in
     *)
    let rec rank i i' = (* i' is the smallest which can be selected first *)
      Goals.create
	(fun i ->
	  if i < n then
	    if i' < n then
	      let j = arg_smallest_release_date i' in
	      (Goals.atomic (swap_bt i j)
		 &&~ Goals.atomic (is_first i)
		 &&~ rank (i+1) (i+1))
	    ||~
	      (Goals.atomic (swap_bt i' j)
		 &&~ Goals.atomic (is_not_first i')
		 &&~ rank i (i'+1))
	    else Stak.fail "Scheduling.Goals.rank"
	  else Goals.success)
	i in
    rank 0 0
end
