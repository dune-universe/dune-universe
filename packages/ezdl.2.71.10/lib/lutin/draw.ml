
(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: draw.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)
(*******************************************************************************)
(*******************************************************************************)

open Value
open Store
open Polyhedron
open List
open Util

(* XXX le code qui suit est foireux. *)
(* En plus, j'y melange les int et float. *)
(* A reprendre !!! *)


(*******************************************************************************)
(*******************************************************************************)

(** [get_subst_from_solved_system store sl] ougth to take in input a list
  of pair "var_name_and_coef, normal_expr" such that the normal_expr
  is reduced to a constant c. It returns the list of substitution
  (Var.name -> c/coef).
*)
let rec (get_subst_from_solved_system : t' ->
	       ((string * Value.num) * Ne.t) list -> (string * Value.num) list) =
  fun store sl ->
    match sl with
	     [] -> []
      | ((vn, v), ne)::tail ->
	       let _ =
	         assert (
	           if
		          (Ne.dimension ne) >= 1
	           then
		          (
		            print_string (t'_to_string  store);
		            print_string "\n*** The expression <<";
		            print_string (Ne.to_string ne) ;
		            print_string ">> ougth to be a constant.\n";
		            flush stdout;
		            false
		          )
	           else
		          true
	         )
	       in
	       let s =
	         match (Ne.find "" ne) with
		          Some(cst) -> (vn, Value.quot_num cst v)
	           | None      -> (vn, Value.diff_num v v) (* return 0 *)
	       in
	       let new_tail =
	         (* We reinject the binding obtained in s into the remaining
	            substitutions *)
	         List.map
	           (fun (x, ne) -> (x,(Ne.apply_simple_subst ne s)))
	           tail			
	       in
            s::(get_subst_from_solved_system store new_tail) 
            

let (deduce_remaining_vars : (string * Value.num) list ->
       t' -> (string * Value.num) list) =
  fun fsl store ->
    (*
       Once the num vars have been drawn, we can deduce the values of
       variables that have substituted when equalities were
       encountered.
    *)
    let sl0 = store.substl' in
    let sl1 = (* apply previously obtained subst *)
      List.map
	(fun (x, ne) ->
	   (*
	     Note that operating left-to-rigth matters here.
	     Indeed, the last substitutions can not depend on
	     variables appearing in the ones obtained before (since
	     once a substitution is added to store.substl, it is
	     applied to the formula), but the opposite is wrong.
	     Typically, sl is of the form :
	     [
             N -> 1;
             N' -> N+1
	     ]
	     therefore, operating rigth-to-left would lead to
	     fail to find the binding of N'.
	   *)
	   (x, List.fold_left (Ne.apply_simple_subst) ne fsl))
	sl0
    in
	get_subst_from_solved_system store sl1


(*******************************************************************************)
(*******************************************************************************)
(* 
   Once we have truncated float to integers, bad solutions migth be 
   obtained. This function try to find a valid solution in the bad 
   drawn solution neighborhood.

   We use at the gravity center of the set of vertices to look
   the rigth direction.

   If it does not find a valid solutuon, it pretends that there is no solution, 
   which migth be wrong. 

   XXX FIXME using linear arithmetics !!!

   Ca, c'est vraiment de la cuisine...
*)
let (find_a_valid_close_point : Var.vnt list -> (int -> string) -> 
      Constraint.ineq list -> point list -> point -> point) =
  fun varl r2n cstrl pl p -> 
    let gravity_center_n = 
      List.fold_left
	     (fun acc p -> List.map2 (fun acci pi -> pi+.acci) acc p)
	     (List.hd pl)
	     (List.tl pl)
    in
    let n = float_of_int (List.length pl) in
    let gravity_center = 
      assert (n <> 0.0);
      List.map (fun g -> g /. n) gravity_center_n
    in
    let modify_point_i gi pi =
      (* cuisine... beurk, j'ai honte... *)
      if pi <> gi then if pi < gi then (pi+.1.0) else (pi-.1.0) else pi
    in
    let new_point = List.map2 modify_point_i gravity_center p in
      if 
	     Polyhedron.point_is_in_poly varl new_point r2n cstrl
      then
	     new_point
      else
	     (
	       (* Well, let's try once more... *)
	       let new_point2 = List.map2 modify_point_i gravity_center new_point in
	         if 
	           Polyhedron.point_is_in_poly varl new_point2 r2n cstrl
	         then
	           new_point2
	         else
	           (
	             (* Well, in that case, it is very likely that the polyhedron is
		             very small, hence ad hoc integer solvers migth work...
	             *)
	             print_string (
		            "### I did not find solutions in the polyhedron which " ^
		              "vertices are : ");
	             Poly_draw.print_points pl;
	             print_string (
		            "### although solutions migth exist. I should call a integer " ^
		              "solver (which is expensive) to be sure...\n");
	             flush stdout;
	             (* XXX Would cut all the choice points in the bdd !!
		             raise No_numeric_solution
		             
		             Therefore, I prefer to return a wrong point + a warning 
		             for the time being...
		             XXX FIXME!!
	             *)
	             print_string (
		            "# Warning: values for variables " ^ 
		              (List.fold_left (fun acc (vn,_vt) -> vn ^ ", " ^ acc) "" varl)
		            ^ " are not valid! (known lucky bug, sorry)\n");
	             flush stdout;
	             p
	           )
	     )  
	
let lcpt = ref 0

let rec (draw_int_loop : int -> Var.vnt list -> Poly_draw.poly_wrap -> 
	       Poly.t -> Constraint.ineq list -> (int -> string) -> point list  -> 
	       point) =
  fun verb varl paral poly cstrl r2n pl ->
    (* Draw until a valid integer point inside the poly is found.

       Try 10 times; then, use an unfair algo.
    *)
    let point = Poly_draw.one_point_poly_wrap paral in
      if
	     (Polyhedron.point_is_in_poly varl point r2n cstrl)
      then
	     ( lcpt:=0; point )
      else
	     (
	       lcpt := !lcpt +1 ;
	       if
	         !lcpt > 10 - (List.length point) 
	           (* do not try too much for high dimension ... *)
	       then
	         (
	           let lgt = List.length pl in
	           let dim = Poly.dim poly in
	           let pl2 = Poly_draw.draw_n_distinct_points (min lgt (dim+1)) pl in
	           let point' = (Poly_draw.draw_point_cheap pl2) in
		          if Polyhedron.point_is_in_poly varl point' r2n cstrl then
		            point'
		          else
		            find_a_valid_close_point varl r2n cstrl pl point' 
	         )
	       else
	         draw_int_loop verb varl paral poly cstrl r2n pl
	     )

(*******************************************************************************)

let (random_num: Num.num -> Num.num -> Num.num) =
  fun min max ->
    let max = Num.float_of_num max and min = Num.float_of_num min in
    let ran_f = min +. Random.float ((max -. min) +. 1.0)  in
    let ran = 
      if ran_f > float_of_int max_int then max_int else
        if ran_f < float_of_int min_int then min_int else (* Raise an error ? *)
          int_of_float ran_f
    in
      Num.Int ran

	
type draw_kind = DrawEdges | DrawInside

(* A generic draw_inside parametrized by the way how points are chosen
   in the list of generators.

   Take care that integers obtained when truncating drawn floats are really
   solutions of the constraints.
*)
let (draw_gen : int -> draw_kind -> t' -> p ->
      Var.num_subst list -> Var.num_subst list) =
  fun verb draw_kind store vntl_poly_rtn_list sl_default ->
    let sl0 =
      (List.fold_left
	      (fun sl_acc (vntl, poly, rank_to_name, cstrl) ->
	         let pl = Polyhedron.get_vertices poly rank_to_name in
	         let lgt = List.length pl in
	         let point =
	           match draw_kind, (snd (hd vntl)) with
		          | DrawEdges, Type.IntT  ->
		              let dim = Poly.dim poly in
		              let pl2 = Poly_draw.draw_n_points (min lgt (dim+1)) pl in
		              let point = Poly_draw.draw_point_cheap pl2 in
		                if 
			               Polyhedron.point_is_in_poly vntl point rank_to_name cstrl
		                then
			               point
		                else
			               let paral = Poly_draw.compute_poly_wrap pl2 in
			                 draw_int_loop verb vntl paral poly cstrl rank_to_name pl2
					             
		          | DrawEdges, Type.FloatT ->
		              (* for the edge heuristic, we do not care to be fair*)
		              let dim = Poly.dim poly in
		              let pl2 = Poly_draw.draw_n_points (min lgt (dim+1)) pl in
		                Poly_draw.draw_point_cheap pl2
                        
		          | DrawInside, Type.IntT ->
		              let paral = Poly_draw.compute_poly_wrap pl in
		                draw_int_loop verb vntl paral poly cstrl rank_to_name pl
                        
		          | DrawInside, Type.FloatT ->
		              let paral = Poly_draw.compute_poly_wrap pl in
		              let point = Poly_draw.one_point_poly_wrap paral in
		                (* Poly_draw.one_point_poly_wrap does not necessary returns
			                a valid solution ! *) 
		                if
			               Polyhedron.point_is_in_poly vntl point rank_to_name cstrl
		                then
			               point
		                else
			               (* In that case, we use the unfair algo. *)
			               let dim = Poly.dim poly in
			               let pl2 = Poly_draw.draw_n_distinct_points (min lgt (dim+1)) pl in
			                 Poly_draw.draw_point_cheap pl2

		          | _,_  -> assert false
	         in
	         let sl = Polyhedron.point_to_subst_list vntl rank_to_name point in
	           rev_append sl sl_acc
	      )
	      []
	      vntl_poly_rtn_list
      )
    in
    let sl1 = rev_append sl0 sl_default in
    let tbl = store.range in
    let sl = rev_append sl1
      (Util.StringMap.fold
	      (fun vn range acc ->
	         ( match range with
	             | RangeI(min, max) -> 
                    let ran = random_num min max in
		                ((vn, I(ran))::acc)
		                  
	             | RangeF(min, max)  ->
		              let n = max -. min in
		              let ran = Random.float n in
                    let f = min +. ran in
		                ((vn, F(f))::acc)
	         )
	      )
	      tbl
	      []
      )
    in
      List.rev_append sl (deduce_remaining_vars sl store)




let (get_valid_vertices : Var.vnt list -> Constraint.ineq list -> Poly.t -> 
       (int -> string) -> Polyhedron.point list) =
  fun vntl cstrl poly rank_to_name -> 
    let pl = Polyhedron.get_vertices poly rank_to_name in
      List.filter
	(fun p -> Polyhedron.point_is_in_poly vntl p rank_to_name cstrl)
	pl
	

let (draw_vertices_one : t' -> p -> Var.num_subst list ->
       Var.num_subst list) =
  fun store vntl_poly_rtn_list sl_default ->
    let sl0 =
      (List.fold_left
	 (fun sl_acc (vntl, poly, rank_to_name,cstrl) ->
	    let pl =
	      let pl = get_valid_vertices vntl cstrl poly rank_to_name in
		if pl = [] then (
		  (* 
		     XXX I should do something like 
		     
		     raise No_numeric_solution 
		     
		     there, but it is currently not handled properly in 
		     Solver.draw (it would cut some branches in the bdd!)
		  *)    
		  Polyhedron.get_vertices poly rank_to_name
		)
		else
		  pl
	    in  
	    let i = try Random.int (List.length pl)  with _ -> assert false
	    in
	    let p = List.nth pl i in
	    let sl = Polyhedron.point_to_subst_list vntl rank_to_name p in
	      rev_append sl sl_acc
	 )
	 []
	 vntl_poly_rtn_list
      )
    in
    let tbl = store.range in
    let sl1 =
      StringMap.fold
	(fun vn range acc ->
	   ( match range with
	       | RangeI(min, max) ->
		   let ran = Random.int 2 in
		     if ran = 0
		     then ((vn, I(min))::acc)
		     else ((vn, I(max))::acc)
	       | RangeF(min, max)  ->
		   let  ran = Random.int 2 in
		     if ran = 0
		     then ((vn, F(min))::acc)
		     else ((vn, F(max))::acc)
	   ))
	tbl
	[]
    in
    let sl = rev_append sl0 (rev_append sl1 sl_default) in
      List.rev_append sl (deduce_remaining_vars sl store)



(* exported *)
let (inside : int -> t' -> p -> int -> Var.num_subst list ->
       Var.num_subst list list) =
  fun verb store poly i sl ->
    Util.unfold (draw_gen verb DrawInside store poly) sl i

let (edges : int -> t' -> p -> int -> Var.num_subst list ->
       Var.num_subst list list) =
  fun verb store poly i sl ->
    Util.unfold (draw_gen verb DrawEdges store poly) sl i


(* exported *)
let (vertices : int -> t' -> p -> int -> Var.num_subst list ->
       Var.num_subst list list) =
  fun _verb store poly i sl ->
    Util.unfold (draw_vertices_one store poly) sl i



(* exported *)
let (get_all_vertices : t' -> p -> Var.num_subst list ->
       Var.num_subst list list) =
  fun store vntl_poly_rtn_list sl_default ->
    let sll_poly =
      (List.fold_left
	 (fun sll_acc (vntl, poly, rank_to_name,cstrl) ->
	    let pl = get_valid_vertices vntl cstrl poly rank_to_name in
	    let pl_all = Polyhedron.get_vertices poly rank_to_name in
	    let sll = 
	      if pl = [] then 
		(* 
		   XXX buggy with integers
		   raise No_numeric_solution 
		*)
		List.map (Polyhedron.point_to_subst_list vntl rank_to_name) pl_all
	      else
		List.map (Polyhedron.point_to_subst_list vntl rank_to_name) pl 
	    in
	      if sll = [] then sll_acc else if sll_acc = [] then sll else
		Util.cartesian_product sll sll_acc (rev_append)
	 )
	 []
	 vntl_poly_rtn_list
      )
    in

    (* The vertices of an interval are its bounds *)
    let tbl = store.range in
    let s_s_l =
      StringMap.fold
	(fun vn range acc ->
	   ( match range with
	       | RangeI(min, max) -> ((vn, I(min)), (vn, I(max)))::acc
	       | RangeF(min, max) -> ((vn, F(min)), (vn, F(max)))::acc
	   )
	)
       tbl
	[]
    in
    let sll =
      if
	s_s_l = []
      then
	(* No solution in the range based solver *)
	sll_poly
      else
	let (sl1, sl2) = List.split s_s_l in
	let sll_range = [sl1;  sl2] in
	let sll0 =
	  if sll_poly = [] then sll_range else
	    Util.cartesian_product sll_range sll_poly rev_append
	in
	  sll0
    in
      if
	sll <> []
      then
	(List.map
	   (fun sl ->
	      (rev_append
		 (rev_append
		    sl
		    (deduce_remaining_vars sl store)
		 )
		 sl_default
	      )
	   )
	   sll
	)
      else
	[(rev_append (deduce_remaining_vars sl_default store) sl_default)]
	
