(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: polyhedron.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

open Value

module StringSet = struct
   include Set.Make(struct type t = string let compare = compare end)
end

let debug = false 

let (get_vars_cl : Constraint.ineq list -> StringSet.t) =
  fun cl ->
    (* get the set of variables contained in a list of inequalities *)
    StringSet.remove ""
      (List.fold_left
         (fun set c ->
	         let vars = Constraint.get_vars_ineq c in
	           List.fold_left
	             (fun set var -> StringSet.add var set)
	             set
	             vars
         )
         StringSet.empty
         cl
      )


let (partition_constraint : Constraint.ineq list list ->
	       Constraint.ineq list list) =
  fun cll ->
    (* Check out the "union-find" algorithm for doing that more efficiently *)
    (Util.union_find
       (fun cl cli ->
	       StringSet.empty <>
	         (StringSet.inter (get_vars_cl cl) (get_vars_cl cli))
       )
       cll
    )
(*     match cll with *)
(*       | [] -> [] *)
(*       | [cl] -> cll *)
(*       | cl::tail -> *)
(* 	  let cll1, cll2 =  *)
(* 	    List.partition *)
(* 	      (fun cli ->  *)
(* 		 StringSet.empty <>  *)
(* 		 (StringSet.inter (get_vars_cl cl) (get_vars_cl cli)) *)
(* 	      ) *)
(* 	      tail *)
(* 	  in *)
(* 	    if  *)
(* 	      cll1 = [] *)
(* 	    then *)
(* 	      cl::(partition_constraint cll2)  *)
(* 	    else *)
(* 	      partition_constraint (List.rev_append cl (List.flatten cll1) :: cll2) *)


open Util

let rec (_power : int -> int -> int) =
  fun n m ->
    let _ = assert (m >= 0) in
      if m = 0 then 1 else n * (_power n (m-1))


let _number_of_ending_zero str =
  (* count the number of '0' at the end of str *)
  let rec aux str i =
    if
      String.get str i = '0'
    then
      aux str (i-1)
    else
      i
  in
  let l = (String.length str) - 1 in
    l - (aux str l)





(* We need to translaste expressions into a string of the format used
   by Polka to parse constraints *)
let rec (ineq_to_polka_string : Constraint.ineq -> string) =
  fun f ->
    let str =
      match f with
	     | Constraint.GZ(ne)   ->
	       ((Ne.to_string_gen (nv_to_string) "+" ne) ^ " > 0 ")
	     | Constraint.GeqZ(ne) ->
	       ((Ne.to_string_gen (nv_to_string) "+" ne) ^ " >= 0 ")
    in
    str
and
    (_constraint_to_string : Constraint.t -> string) =
  fun f ->
    let str =
      match f with
	     | Constraint.Bv(var)  -> (Var.name var)
	     | Constraint.Ineq(ineq) -> ineq_to_polka_string ineq
	     | Constraint. EqZ(ne)  ->
	       ((Ne.to_string_gen (nv_to_string) "" ne) ^ " = 0 ")
    in
    str
and
    (nv_to_string : Value.num -> string) =
  fun n ->
    match n with
	   | I(i) -> Num.string_of_num i
      | F(f) -> float_to_polka_string f
and
    (_int_to_string : int -> string) =
  fun i ->
    ((if i >= 0 then "+" else "-") ^ (string_of_int (abs i)))
and
    (* I need to convert the float '5236.2556' into '+52362556/10000' for polka *)
    (float_to_polka_string : float -> string) =
  fun f ->
    let res =
      match f with
        | 0.0 -> "+0"
        | 1.0 -> "+1"
        | -1.0 -> "-1"
        | _ ->
          let sign = if f >= 0.0 then "+" else "-" in
          let f = abs_float f in
          let str = my_string_of_float f in
          let dot_index = String.index str '.' in
          let l = String.length str in
          let card_zero_denom = l-dot_index-1 in
          let denom = "1"^ (String.make card_zero_denom '0') in
          let numerator = 
          (* remove the "." from str *)
            (String.sub str 0 (dot_index)) ^  (String.sub str (dot_index+1) (card_zero_denom))
          in
          sign ^ numerator ^ "/" ^ denom 
    in
    (* Printf.eprintf "Converting %f into the rat %s\n" f res; *)
    (* flush stderr; *)
    res



(****************************************************************************)
(****************************************************************************)

type range =
    RangeI of Num.num * Num.num     (* min and max *)
  | RangeF of float * float (* Ditto for floats *)

let (range_to_string : range -> string) =
  fun r ->
    match r with
      | RangeI(min, max) ->
	  ("[" ^ (Num.string_of_num min) ^ "," ^ (Num.string_of_num max)^ "]")
      | RangeF(min, max) ->
	  ("[" ^ (string_of_float min) ^ "," ^ (string_of_float max)^ "]")


let already_initialized = ref false
let polka_init = ref 100
let poly_cache_tbl = Hashtbl.create 1
let polka_dim = ref 0

let clean_internal_tbl () =
  Hashtbl.clear poly_cache_tbl
    
let rec (build_poly_list_from_delayed_cstr : int -> range Util.StringMap.t ->
          Constraint.ineq list ->
          range Util.StringMap.t *
            Var.name list *
            (Var.vnt list * Poly.t * (int -> string) * Constraint.ineq list) list ) =
  fun verb tbl delayed ->
    try
      hfind poly_cache_tbl (tbl, delayed)
    with Not_found ->
      try
	     let cll = partition_constraint (List.map (fun c -> [c]) delayed) in
	     let all_vars = get_vars_cl delayed in
	     let max_dim =
	       List.fold_left
	         (fun acc _cl ->
	            let var_nb = StringSet.cardinal all_vars in
		           if var_nb > acc then var_nb else acc
	         )
	         0
	         cll
	     in  
	     let res =
          if max_dim+3 > !polka_dim then (
            polka_dim := max_dim+3;
            already_initialized := false
          );
	       if Util.hashtbl_size poly_cache_tbl > 100 then
	         (* Do not let that table explode...*)
	         Hashtbl.clear poly_cache_tbl;
	       if !already_initialized then () else
	         (
              (*               print_string ("# Polka.initialize "^ *)
              (*                               (string_of_int !polka_dim) ^ " " ^ *)
              (*                               (string_of_int !polka_init) ^"\n"); *)
              (*               flush stdout; *)
	           Polka.initialize ~strict:true ~maxdims: !polka_dim ~maxrows: !polka_init;
	           already_initialized := true
	         );
	       List.map
	         (fun cl ->
	            let vars_of_cl = get_vars_cl cl in
	            let dim = StringSet.cardinal vars_of_cl in
	            let rank_of_name_tbl = Hashtbl.create 0
	            and name_of_rank_tbl = Hashtbl.create 0 in
	            let _ =
		           StringSet.fold
		             (fun var i ->
		                Hashtbl.add rank_of_name_tbl var i;
		                Hashtbl.add name_of_rank_tbl i var;
		                (i+1)
		             )
		             vars_of_cl
		             0
	            in
	            let (name_to_rank : string -> int) = hfind rank_of_name_tbl
	            and (rank_to_name : int -> string) = hfind name_of_rank_tbl
	            in
               let lfind var tbl =
                 try Util.mfind var tbl 
                 with Not_found -> 
                   print_string (
                     "Internal error: don't find the value of " ^ (var)
                     ^ " in: " ^ (String.concat "," 
                                    (StringMap.fold (fun v _ acc -> (v)::acc) tbl [])) ^
                       "\n");
                   flush stdout;
                   assert false
               in
	            let (vntl0, poly0, cl_init) =
		           StringSet.fold
		             (fun  var (vntl, poly, cl_init)->
		                (*
			               Add the constraint present in tbl to the polyhedron
			               for each variables in [vars_of_cl]
		                *)
		                let (ineq_min, ineq_max, vnt) =
			               match (lfind var tbl) with
			                   RangeI(min, max) ->
			                     (
				                    (Constraint.GeqZ
				                       (Ne.diff
				                          (Ne.make var (I (Num.num_of_int 1)))
				                          (Ne.make "" (I min)))),
				                    (Constraint.GeqZ
				                       (Ne.diff
				                          (Ne.make "" (I max))
				                          (Ne.make var (I  (Num.num_of_int 1))))),
				                    (var, Type.IntT)
			                     )
			                 | RangeF(min, max) ->
			                     (
				                    (Constraint.GeqZ
				                       (Ne.diff
				                          (Ne.make var (F 1.))
				                          (Ne.make "" (F min)))),
				                    (Constraint.GeqZ
				                       (Ne.diff
				                          (Ne.make "" (F max))
				                          (Ne.make var (F 1.)))),
				                    (var, Type.FloatT)
			                     )
		                in
		                let ineq_min_str = (ineq_to_polka_string ineq_min)
		                and ineq_max_str = (ineq_to_polka_string ineq_max)
		                in
		                let v1 =
                        if debug then 
                          (
                            print_string (" ==> " ^ ineq_min_str ^ "\n"); 
                            flush stdout
                          );
			               Vector.of_constraint (name_to_rank) dim ineq_min_str
		                and v2 =
                        if debug then  
                          (
                            print_string (" ==> " ^ ineq_max_str ^ "\n"); 
                            flush stdout
                          );  
			               Vector.of_constraint (name_to_rank) dim ineq_max_str
		                in
			               (
			                 vnt::vntl,
			                 Poly.add_constraint (Poly.add_constraint poly v1) v2,
			                 ineq_min::ineq_max::cl_init
			               )
		             )
		             vars_of_cl
		             ([], (Poly.universe dim), [])
	            in
	            let poly1 =
		           List.fold_left
		             (fun poly ineq ->
		                (* add the constraint [ineq] to polyhedron [poly] *)
		                let ineq_str = ineq_to_polka_string ineq in
		                let v = 
                        if debug then 
                          (print_string (" ==> " ^ ineq_str ^ "\n");flush stdout); 
                        (Vector.of_constraint (name_to_rank) dim ineq_str) in
                      let res = 
			               Poly.add_constraint poly v
                      in
                        if debug then (
                          print_string (" End of "^ineq_str^"\n");flush stdout); 
                        res  
		             )
		             poly0
		             cl
	            in
                 (vntl0, poly1, rank_to_name, List.rev_append cl cl_init)
	         )
	         cll
	     in

	     let all_vars_list = StringSet.elements all_vars in
	     let tbl2 =
	       List.fold_left
	         (fun acc var -> StringMap.remove var acc)
	         (* remove delayed constraints from the store *)
	         tbl
	         all_vars_list
	     in
	       Hashtbl.add poly_cache_tbl (tbl, delayed) (tbl2, all_vars_list, res);

	       if verb >= 2 then (
	         print_string (
	           "\n*** " ^
	             (string_of_int (List.length res)) ^ 
	             " polyhedra was/were used. " ^
	             "Variables are grouped as follows: \n";
            );
	         List.iter
	           (fun (vntl, _, _, cl) -> 
		           print_string " ==> ";
		           List.iter (fun (vn,_) -> print_string (vn ^ ", ")) vntl;
		           print_string " which are constraint by: \n\t";
		           List.iter 
		             (fun c -> 
		                print_string ((Constraint.ineq_to_string c) ^ " and \n\t"))
		             cl;
		           print_string "\n"
	           )
	           res;
	         flush stdout;
	       );
	       (tbl2, all_vars_list, res)
	         
      with 

	     | Failure  "poly: cannot create polyhedron with null, negative dimension, or dimension greater than polka_maxcolumns-polka_dec\n" [@warning "-52"]
	     | Failure "Chernikova: out of table space\n" [@warning "-52"] ->
	         output_string stderr ("Error in Polka: " ); 
	         output_string stderr "\nNot enough memory for Polka -> doubling the mem size...\n";
	         flush stderr;
	         polka_init := 2 * !polka_init;
	         already_initialized := false;
	         build_poly_list_from_delayed_cstr verb tbl delayed

	     | Failure e -> 
	         output_string stderr ("Error in build_poly_list_from_delayed_cstr:\n   " ^ e ^ "\n");
	         flush stderr;
	         assert false
	    

(****************************************************************************)
(****************************************************************************)

type point = float list




let (matrix_to_vector_list : Matrix.t -> Vector.t list) =
  fun m ->
    let rec matrix_to_vector_list2 cpt acc =
      let nr = Matrix.nbrows m in
	if
	  cpt = (nr-1)
	then
	  (Matrix.get_row m cpt)::acc
	else
	  matrix_to_vector_list2 (cpt+1) ((Matrix.get_row m cpt)::acc)
    in
      matrix_to_vector_list2 0 []


let get_row r i = 
  let str = Vector.get_str10 r i in
    float_of_string str


let (get_vertices : Poly.t ->  (int -> string) -> point list) =
  fun poly rank_to_name ->
    (*
      Returns the vertices of the polyhedron defined by [store]
      if it is a polytope, fails otherwise.
    *)
    (*     let _ =  *)
    (*       print_string " ===> Minimisation !!!\n"; *)
    (*       flush stdout ; *)
    (*       Poly.minimize poly ; *)
    (*       Poly.print rank_to_name Format.std_formatter  poly ;  *)
    (*       print_string "\n"; *)
    (*       flush stdout ; *)
    (*     in  *)
    let res = 
      match Poly.frames poly with
	       Some(matrix) ->
	         let row_list = matrix_to_vector_list matrix in
	         let generators =
	           List.map
		          (fun row ->
		             let denom = get_row row 1
		             and line  = get_row row 0
		             in
		               if
		                 (line = 0.0 || denom = 0.0)
		               then
		                 (
			                Poly.print rank_to_name Format.std_formatter poly ;
			                print_string "\n";
			                flush stdout ;
			                failwith ("*** Can not draw fairly in an " ^
				                         "unbounded set of solutions.")
		                 )
		               else
		                 let rec (vector_to_point : Vector.t -> float -> int -> point) =
			                fun row denom cpt ->
			                  let p = ( (get_row row cpt)) /. denom in
			                    if
			                      (cpt = (Vector.length row) -1)
			                    then
			                      [p]
			                    else
			                      p::(vector_to_point row denom (cpt+1))
		                 in
			                vector_to_point row ( denom) 2
		          )
		          row_list
	         in
	           (* Remove the epsilon dimension *)
	           List.map
		          (fun l -> List.tl l)
		          (List.filter
		             (fun l -> (List.hd l) = 0.)
		             generators
		          )
        | None -> assert false
    in      
(*     let sop p = "("^ (String.concat "," (List.map string_of_float p)) ^ ")" in *)
(*     let str = String.concat ","  (List.map sop res) in *)
(*       Printf.eprintf "Generators are: %s.\n" str; *)
(*       flush stderr; *)
      res


(****************************************************************************)
(****************************************************************************)

(* exported *)
let (point_to_subst_list : Var.vnt list -> (int -> string) -> point ->
       Var.num_subst list) =
  fun vntl rank_to_name p ->
    snd
    (List.fold_left
       (fun (j, sl) x ->
	  let vn = rank_to_name j in
	  let vt = List.assoc vn vntl in
	  let s =
	    match vt with
	      | Type.FloatT -> (vn, F(x))
	      | Type.IntT  ->
		       (* XXX brrrr, this is wrong! But id is checked later if the
		          drawn point is a valid solution.  *)
		       let i = 
		         if (abs_float x) > (float_of_int max_int) then 
		           failwith "Float too big; can not convert it into a integer.";
		         if (x -. (floor x) < 0.5) then
			        truncate (floor x)
			      else
			        (* we chose the closest of ceil and floor *)
			  truncate (ceil x)
		       (* I need to call floor before truncate because
			       otherwise, around 0, it does not work consistently.  *)
		  in
		    (vn, I(Num.num_of_int i))
		      
	      | _ ->
		       assert false
	  in
	    (j+1, s::sl)
       )
       (0, [])
       p
    )

(****************************************************************************)
(****************************************************************************)

let (_point_to_vector : point -> Vector.t) =
  fun point ->
    let row = Vector.make ((List.length point) + 3) in
    let denom = 1.0 /. !(Util.eps) in
    let point_int = List.map (fun p -> int_of_float (p *. denom)) point in
    let i = ref 3 in
      Vector.set row 0 1; (* it is not a line *)
      Vector.set row 1 (int_of_float denom);
      Vector.set row 2 0; (* this is the epsilon dimension *)
      List.iter
	     (fun p -> Vector.set row !i p; i:=!i+1)
	     point_int;
      row

(* exported *)
let (point_is_in_poly : Var.vnt list -> point -> (int -> string) ->
       Constraint.ineq list -> bool) =
  fun varl p r2n cstrl ->
    let s = point_to_subst_list varl r2n p in
      List.for_all (Constraint.eval_ineq s) cstrl

(* let (point_is_in_poly : point -> Poly.t -> bool) = *)
(*   fun p poly ->  *)
(*     let v = point_to_vector p in *)
(*       Poly.is_generator_included_in v poly  *)



let (volume: Poly.t -> (int -> string) -> float) =
  fun poly r2n ->
  let pl = get_vertices poly r2n in
  let paral = Poly_draw.compute_poly_wrap pl in
  let parallogram_vol = Poly_draw.poly_wrap_volume paral in

(*   let ni = 100 in *)
(*   let nf = float_of_int ni in *)
(*   let random_points = Poly_draw.n_points_poly_wrap o vl ni in *)
(*   let pertencage_in =  *)
(*     max *)
(*       1. (* even if no points appear to be in [poly], at least we pretend  *)
(* 	   that the volume of poly is 1% the volume of the unit cube. *) *)
(*       (List.fold_left  *)
(* 	 (fun cpt p -> if point_is_in_poly p poly then cpt +. 1. else cpt) *)
(* 	 0. *)
(* 	 random_points *)
(*       ) *)
(*   in *)

  (* The volume of the poly_wrap is not a so bad approximation after all *)
  let volume =
    parallogram_vol
(*     *. (pertencage_in /. nf)  *)
  in
(*     print_string ( *)
(*       " " ^ (string_of_float (pertencage_in /. nf *. 100.0)) ^ "  "); *)
(*     flush stdout; *)
    volume

