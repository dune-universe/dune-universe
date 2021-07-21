(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: store.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

open Exp
open Value
open Constraint
open Polyhedron
open Util


let debug_store = false
let debug_store2 = false

(*  let debug_store = true  *)
(*  let debug_store2 = true   *)

type p = (Var.vnt list * Poly.t * (int -> string) * Constraint.ineq list) list

type range_store =  Polyhedron.range Util.StringMap.t



type t = {
  (*
    This field is used to store where each variable ranges.  It is
    set to [Unsat] when the system becomes unsatisfiable, namely,
    when the range for one of the variable becomes empty.

    Some variables are represented by Ranges (polyhedron of dimension
    one). Some others by plain Polyhedron. The idea is that, at bdd
    leaves, if it remains some delayed constraints, we switch to a
    polyhedron representation.  *)
  var : vars_domain ;

  (*
    This field is used to substitute a variable by an expression. This
    is to deal with equalities: when an equality is encountered,
    we can remove one dimension by putting the equality into a
    such a substitution.

    Then we apply it to all the other relations. the value of the
    substituted variable is then obtained once the other var have
    been drawn.

    We add an element to this list if
    - an equality is encountered during the drawing/bdd-traversal
    - whenever a variable become bounded (1) after a constraint is
      added to the store

    (1) i.e., when the interval is reduced to one single point
  *)
  substl : Ne.subst list;

  (*
    When the dimension of an atomic formula is greater than 1, we
    delay its addition to the store until an equality makes it a
    constraint of dimension 1 (i.e., it contains only 1 var). At bdd
    leaves, if this list is not empty, it means that the current
    formula cannot be solved with an interval based solver.
    In that case, we use a polyhedron solver.
  *)
  delay : Constraint.ineq list ;

  (* Variables that have been constrained. If a formula has not been
    constraint when the draw is done, we give it its default value if any.
  *)
  untouched : Exp.var list
}
and 
  vars_domain =
    Unsat of Constraint.t * t
  | Range of range_store
      

(** contains basically the same info as [t] with a few fields removed *)
type t' = {
  range : range_store ;
  substl' : Ne.subst list;
  untouched' : Exp.var list
}


let unsat_store cstr store= 
  { var = Unsat(cstr,store) ; substl = [] ; delay = [] ; untouched = [] }

let (rm :  Exp.var list -> Var.name -> Exp.var list) =
  fun varl vn ->
    (* Removes in [varl] all the vars which names begins with [vn].

       Indeed, when a structured variable "s" is touched, then
       so do the variables "s.f1", "s.f2[1]", ... 
    *)
    let is_not_a_prefix_of vn v =
      let res = 
	let l_vn = String.length vn in
	let l = String.length v in
	  if l_vn > l then true else (String.sub v 0 l_vn) <> vn 
      in
	 res
    in
      (List.filter (fun v -> is_not_a_prefix_of vn (Var.name v)) varl)


let (remove_var_from_range_store : t' -> Exp.var -> t') =
  fun st var ->
    {
      range = StringMap.remove (Var.name var) st.range;
      substl' = st.substl';
      untouched' = st.untouched'
    }


let (get_untouched_var : t' -> Exp.var list) =
  fun st ->
    st.untouched'


(*
   XXX ougth to be modifiable from the outside.
   nb : if those values are too big, sim2chro crashes ....
*)
(* let default_min_float = -10000. *)
(* let default_max_float = 10000. *)
(* let default_max_int = 10000 *)
(* let default_min_int = -10000 *)
(*
   XXX What should be the default values ???
   Too big values migth break other tools (e.g., sim2chro...)
*)
let lucky_max_int = (max_int / 4)

let default_max_float = (float_of_int lucky_max_int)
(*   (float_of_int max_int) /. 2.**(float_of_int (!Util.precision + 1)) *)
let default_min_float = (-. default_max_float)
let default_max_int = (Num.Int lucky_max_int)
let default_min_int = (Num.Int (-lucky_max_int))
let zero  = Num.Int 0

let one_of_num = function
  | I _ -> I (Num.Int 1)
  | F _ -> F 1.0

(* exported *)
let (create : Exp.var list -> t) =
  fun var_l ->
    let (add_one_var : range_store * Ne.subst list -> Exp.var ->
         range_store * Ne.subst list) =
      fun (tbl,sl) var ->
        let to_num_opt = function 
          | Some(Numer(Ival(min))) -> Some (Ival(min))
          | Some(Numer(Uminus(Ival(min)))) -> Some (Ival(Num.minus_num min))
          | Some(Numer(Fval(min))) -> Some (Fval(min))
          | Some(Numer(Uminus(Fval(min)))) -> Some (Fval(-.min))
          | None -> None
          | _  -> output_string stderr
                    "Only immediate constant are allowed in variable ranges.\n";
            flush  stderr;
            assert false
        in
	     let range =
	       match (to_num_opt (Var.min var)), (to_num_opt  (Var.max var)) with
	       | Some(Ival(min)), Some(Ival(max)) -> RangeI(min, max)
	       | None,            Some(Ival(max)) -> RangeI(default_min_int, max)
	       | Some(Ival(min)), None            -> RangeI(min, default_max_int)
	       | Some(Fval(min)), Some(Fval(max)) -> RangeF(min, max)
	       | None,            Some(Fval(max)) -> RangeF(default_min_float, max)
	       | Some(Fval(min)), None            -> RangeF(min, default_max_float)
	       | None, None -> (
		        match Var.typ var with
		          Type.IntT -> RangeI(default_min_int, default_max_int)
		        | Type.FloatT -> RangeF(default_min_float, default_max_float)
		        | _ -> assert false
	         )
	       | _ -> 
            print_string ((Var.to_string var) ^ "\n"); flush stdout;
	         assert false
	     in
        let subst_opt = (* sometimes, range are actually subst ! *)
          match range with
          | RangeI (n1,n2) -> if n1=n2 then Some (I n1) else None
          | RangeF (n1,n2) -> if n1=n2 then Some (F n1) else None
        in
        match subst_opt with
        | Some v -> tbl, ((Var.name var, one_of_num v), Ne.make "" v)::sl
        | None  ->  StringMap.add (Var.name var) range tbl, sl 
    in
    let tbl,sl = List.fold_left (add_one_var) (StringMap.empty,[]) var_l in
    {
      var = Range(tbl) ;
      substl = sl;
      delay = [];
      untouched = var_l
    }


(* Normalised atomic constraints *)
type nac =
  | NSupF   of float (** >  *)
  | NSupEqF of float (** >= *)
  | NInfF   of float (** <  *)
  | NInfEqF of float (** <= *)
  (*   | NEqF    of float (** = *) *)

  | NSupEqI of Num.num (** >=  *)
  | NInfEqI of Num.num (** <=  *)
(*   | NEqI    of Num.num (** = *) *)


(****************************************************************************)
(* Pretty printing   *)


let (range_to_string : range -> string) =
  fun range ->
    match range with
    RangeI(min, max) ->
      ("[" ^ (Num.string_of_num min) ^ ", " ^ (Num.string_of_num max) ^ "] ")
  | RangeF(min, max) ->
      ("[" ^ (string_of_float min) ^ ", " ^ (string_of_float max) ^ "] ")

(* exported *)
let (to_string : t -> string) =
  fun s ->
    let var_str =
      ("\n*** Variable ranges: \n" ^
         match s.var with
	          Unsat(_,_) -> "Empty store"
	        | Range(tbl) ->
	            (StringMap.fold
	               (fun vn range acc ->
		               ("   " ^ vn ^ " in " ^ (range_to_string range) ^ "\n" ^ acc)
	               )
	               tbl
	               "\n"
	            )
		)
    and substl_str = 
      if s.substl = [] then "" else
        ("\n*** Substitutions: \n" ^ Ne.substl_to_string s.substl)
    and delay_str = 
      if s.delay = [] then "" else
	     ("\n*** Delayed constraints: \n" ^
           List.fold_left
	        (fun acc d -> acc ^ "\n" ^ (Constraint.ineq_to_string d))
	        ""
	        s.delay)
    in
      (var_str ^ substl_str ^ delay_str)
and
    (t'_to_string : t' -> string) =
  fun s ->
    let var_str = (
      "\n*** Variable ranges: \n" ^
        (StringMap.fold
	        (fun vn range acc ->
	           ("   " ^ vn ^ " in " ^ (range_to_string range) ^ "\n" ^ acc)
	        )
	        s.range
	        "\n"
        )
    )
    and substl_str = ("\n*** Substitutions: \n" ^ Ne.substl_to_string s.substl')
    in
      (var_str ^ substl_str)

let (print_store : t -> unit) =
  fun s ->
    Format.print_string (to_string s)

(****************************************************************************)

(*
   Note that we check the satisfiability of constraints over
   polyhedra at bdd leaves, which, in some circumstances, migth be
   inefficient. The point is that, if we chose to check the formula
   satisfiability during the bdd traversal, we take the risk that a
   very big polyhedron is created whereas it was not necessary (because
   of forthcoming equalities that would reduce its dimension). And
   creating polyhedron with too big (>15) dimensions simply runs
   forever, which is really bad.
*)

(* exported *)
exception No_polyedral_solution

let (switch_to_polyhedron_representation_do : int -> t -> t' * p) =
  fun verb store ->
    (* handle delayed constraints using polyhedron *)
    match store.var with
	   Unsat(_,_) ->
	   print_string ("\nZZZ Dead code reached, oups...\n") ;
      flush stdout;
	   raise No_polyedral_solution (* this ougth to be dead code ... *)
    | Range tbl ->
	   if
	     store.delay = []
	   then
	     (
	       { range = tbl; substl' = store.substl; untouched' = store.untouched }
	       ,
	       []
	     )
	   else
	     let (tbl2, touched_vars, poly_l) =
	       (* side effect: this function also removes from [store] variables
		       that are put into the polyhedron store *)
	       Polyhedron.build_poly_list_from_delayed_cstr verb tbl store.delay
	     in
	     List.iter
		    (fun (_, poly, _, _) ->
		       if Poly.is_empty poly then (
		         if debug_store then (
		           print_string (to_string  store);
		           print_string "\n The polyhedron is empty .\n";
		           flush stdout );
		         raise No_polyedral_solution
             )
		    )
		    poly_l;

	     (
		    { range = tbl2; substl' = store.substl ;
		      untouched' = List.fold_left (rm) store.untouched touched_vars
		    }
		    ,
		    poly_l
	     )

(* tabulate the result as this translation is expensive *)
let poly_table = ref (Hashtbl.create 1)
let poly_table_size = ref 0

(* exported *)
let (switch_to_polyhedron_representation : int -> t -> t' * p) =
  fun verb store ->
    (Util.tabulate_result
       poly_table poly_table_size 
       100 switch_to_polyhedron_representation_do verb store)


(****************************************************************************)
(****************************************************************************)

let (compute_volume_do : int -> t -> float) =
  fun verb store ->
    let eps = !(Util.eps) in
    let factor = 1.0 /. eps in
      (*
	     In order to compare the number of solutions in a integer polyhedron
	     and in a float one, we multiply the volume of the float polyhedron by
	     2 ^ precision.
      *)
      match store.var with Unsat(_,_)  ->  0.0 | _ -> 
	     let (store', poly_l) = switch_to_polyhedron_representation verb store in
	     let range_vol =
	       Util.StringMap.fold
	         (fun _vn r acc ->
	            match r with
		           | RangeI(min, max) ->
		               acc *. (Num.float_of_num (Num.succ_num (Num.sub_num max min)))
		                 
		           | RangeF(min, max) ->
		               acc *.  (max -. min +. eps) *. factor
	         )
	         store'.range
	         1.0
	     in
	     let poly_vol =
	       List.fold_left
	         (fun acc (_,p,r2n,_) -> acc *. factor *. (Polyhedron.volume p r2n))
	         1.0
	         poly_l
	     in
	       range_vol *. poly_vol
	

(* tabulate the result of the volume computation *)
let store_volume = ref (Hashtbl.create 1)
let store_volume_size = ref 0

let (compute_volume : int -> t -> float) =
  fun verb store ->
    let volume =
      (Util.tabulate_result
	      store_volume store_volume_size 100 compute_volume_do verb store)
    in
      if debug_store then
	     (
	       print_string ( 
	         " ******* The store \n" ^
	           (to_string store) ^ " has volume " ^ (string_of_float volume) ^ "\n");
	       flush stdout;
	     );
      volume
		

(****************************************************************************)

let (_div : int -> int -> int) =
  fun x y ->
    (* I define my own integer division as the division of Pervasives
       does not consistently rounds its result (ie, the result is
       round to the least integer if it is positive, and to the
       greatest integer if it is negative). *)
    let xf = float_of_int x
    and yf = float_of_int y
    in
      int_of_float (floor (xf /.yf))


let (normalise : Constraint.ineq -> Var.name * nac ) =
  fun cstr ->
    (* Transform atomic formula into a data type that is easier to
       process.

       Fails if [cstr] contains more than one variable (in which
       case the constraint should have been delayed).
    *)
    let (get_vn_and_constant : Ne.t -> ( (* ne = ax+b*)
	        Value.num  (* The constant b *)
	        * Value.num  (* The coefficient of the variable a *)
	        * Var.name   (* The name of the variable x *)
	      )
	     ) =
      fun ne ->
	     let list = Ne.fold (fun vn num acc -> (vn,num)::acc) ne [] in
	       match list with
	           (* 0 var *)
	           [("", cst)] ->
		          ( match cst with
		                I(_) -> (cst, I(Num.Int 0) , "")
		              | F(_) -> (cst, F(0.), "")
		          )

	         (* 1 var *)
	         | [("", cst); (vn, coeff)] -> (cst, coeff, vn)
	         | [(vn, coeff); ("", cst)] -> (cst, coeff, vn)
	         | [(vn, coeff)] ->
		          ( match coeff with
		                I(_) -> (I(Num.Int 0), coeff, vn)
		              | F(_) -> (F(0.), coeff, vn)
		          )

	         (* more than 1 var *)
	         | _ ->
		          assert false
    in
      match cstr with
	       GZ(ne) -> (* coeff.x + cst > 0 *)
	         let (cst, coeff, vn) = get_vn_and_constant ne in
	           ( match (cst, coeff) with
		              (I(i_cst), I(i_coeff)) ->  
		                let i = Num.quo_num (Num.minus_num i_cst)  i_coeff in
			               if Num.gt_num i_coeff zero then (vn, NSupEqI(Num.succ_num i))
			               else
			                 if Num.eq_num (Num.mod_num i_cst i_coeff)  zero
			                 then (vn, NInfEqI(Num.pred_num i))
			                 else (vn, NInfEqI(i))

		            | (F(f_cst), F(f_coeff)) ->
		                if f_coeff > 0.
		                then (vn, NSupF(-.f_cst /. f_coeff))
		                else (vn, NInfF(-.f_cst /. f_coeff))

		            | (I(i), F(f)) ->
		                failwith ("*** Error: " ^ (Num.string_of_num i)
				                    ^ " is an integer and "
				                    ^ (string_of_float f) ^ " is a float.\n")
		            | (F(f), I(i)) ->
		                failwith ("*** Error: " ^ (Num.string_of_num i)
				                    ^ " is an integer and "
				                    ^ (string_of_float f) ^ " is a float.\n")
	           )

	     | GeqZ(ne) ->
	         let (cst, coeff, vn) = get_vn_and_constant ne in
	           ( match (cst, coeff) with
		              (I(i_cst), I(i_coeff)) ->
		                let i =  Num.quo_num (Num.minus_num i_cst)  i_coeff in
			               if Num.gt_num i_coeff  zero
			               then
			                 if Num.eq_num (Num.mod_num i_cst i_coeff)  zero
			                 then (vn, NSupEqI(i))
			                 else (vn, NSupEqI(Num.succ_num i))
			               else
			                 if Num.eq_num (Num.mod_num i_cst i_coeff) zero
			                 then (vn, NInfEqI(i))
			                 else (vn, NInfEqI(i))
		            | (F(f_cst), F(f_coeff)) ->
		                if f_coeff > 0.
		                then (vn, NSupEqF(-.f_cst /. f_coeff))
		                else (vn, NInfEqF(-.f_cst /. f_coeff))
		            | (I(_), F(_)) -> assert false
		            | (F(_), I(_)) -> assert false
	           )




let (make_subst : Var.name -> Value.num -> Ne.subst) =
  fun vn value ->
    (* returns a subst from [vn] to [value] *)
    match value with
	I _ -> ((vn, (I (Num.Int 1))), Ne.make "" value)
      | F _ -> ((vn, (F 1.)), Ne.make "" value)



(** if a constraint [cstr] = [GeqZ(ne)] is such that the store
  contains the constraint [eqZ(-ne)] among its delayed variables,
  then [cstr] turns out to be an equality. In that case, this
  function returns [ne] as well as the store with the delayed
  constraint [eqZ(-ne)] removed. *)

(* let (is_ineq_cstr_an_eq : Constraint.t -> t -> (t * Ne.t) option) = *)
(*   fun cstr store -> *)
(*     let ne = *)
(*       match cstr with *)
(* 	  GZ(ne) -> ne *)
(* 	| GeqZ(ne) -> ne *)
(* 	| _ -> assert false *)
(*     in *)
(*     let rec get_cstr ne d acc = *)
(*       match d with *)
(* 	  [] -> raise Not_found *)
(* 	| GZ(ne2)::dtail ->  *)
(* 	    if ne = ne2 *)
(* 	    then GZ(ne2), List.rev_append acc dtail *)
(* 	    else get_cstr ne dtail (GZ(ne2)::acc) *)
(* 	| GeqZ(ne2)::dtail -> *)
(* 	    if ne = ne2 *)
(* 	    then GeqZ(ne2), List.rev_append acc dtail *)
(* 	    else get_cstr ne dtail (GeqZ(ne2)::acc) *)
(* 	| _ -> assert false *)
(*     in *)
(*     let d2 = *)
(*       try  *)
(* 	match  cstr, (get_cstr ne d []) with *)
(* 	  | GeqZ(ne), (GZ(_),  d2) -> GZ(ne)::d2 *)
(* 	      (* Indeed, GeqZ(ne) => GZ(ne) *) *)
(* 	  | _ -> d *)
(* 	with  *)
(* 	    Not_found -> d *)
(* 	in *)
(* 	let new_d = *)
(* 	  try  *)
(* 	    if *)
(* 	      match cstr, (get_cstr (Ne.neg ne) d []) with *)
(* 		  GeqZ(ne), (GeqZ(_), d2) -> true *)
(* 		| _ -> false *)
(* 	    then *)
(* 	       *)
(* 	    else *)
(* 	       *)
(* 	  with  *)
(* 	      Not_found -> d *)
(* 	in *)


(* exported *)
let rec (add_constraint : t -> Formula_to_bdd.t -> Constraint.t -> t) =
  fun store bddt cstr0 ->
    let cstr = Constraint.apply_substl store.substl cstr0 in
    let _ =
      if debug_store2 then (
	     print_string (
	       "add_constraint (" ^
	       (string_of_int 
	          (Formula_to_bdd.get_index_from_linear_constraint bddt cstr0)) ^
	       ") " ^
	       (Constraint.to_string cstr) ^ " \n");
	     flush stdout
      );
      if debug_store then (
	     print_string "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n " ;
	     print_string ">>>> The store before adding: " ;
	     print_string (Constraint.to_string cstr);
	     print_string (to_string  store);
	     flush stdout
      );
    in
    let eps = !(Util.eps) in
    let (var, sl, d, uvars) =
      (store.var, store.substl, store.delay, store.untouched)
    in
    let dim = Constraint.dimension cstr in
    let res =
      if
	     dim = 0
      then
	     ( match cstr with
	         EqZ(ne) -> add_eq_to_store store bddt ne
	       | Bv _ -> assert false
	       | Ineq(GZ(ne)) ->
		      if
		        ( match (Ne.find "" ne) with
			         Some v -> Value.num_sup_zero v
		          | None -> false
		        )
		      then
		        store
		      else (
		        if debug_store2 then (
		          print_string ( 
		            "\nAdding constraint " ^ (Constraint.to_string cstr) ^ 
		            " leads to an empty store.\n") ;
		          flush stdout);
		        unsat_store (Ineq(GZ(ne))) store
		      )
	       | Ineq(GeqZ(ne)) ->
		      if
		        ( match (Ne.find "" ne) with
			         Some v -> Value.num_supeq_zero v
		          | None -> true
		        )
		      then
		        store
		      else (
		        if debug_store2 then (
		          print_string (
		            "\nAdding constraint " ^ (Constraint.to_string cstr) ^ 
		            " leads to an empty store.\n") ;
		          flush stdout);
		        unsat_store (Ineq(GeqZ(ne))) store
		      )
	     )
      else if
	     dim > 1
      then
  (*
We could also choose not to delay those constraints and
switch to the polyhedron representation for the concerned
variables there.

What is the better solution really ???
*)
	     ( match cstr with
	         EqZ(ne) -> add_eq_to_store store bddt ne
	       | Bv _ -> assert false
	       | Ineq ineq ->
		      if debug_store then (
		        let cstr_str = (Constraint.to_string cstr) in
		        print_string "\n ==> delay  " ;
		        print_string cstr_str;
		        flush stdout
		      );
		      { var=var ; substl=sl ; delay = ineq::d  ;
		        untouched = uvars}
	     )
      else
	     (* dim = 1 *)
	     match store.var with
	       Unsat(cstr, store) ->
	       if debug_store2 then 
		      print_string (
		        "\nAdding constraint " ^ (Constraint.to_string cstr) ^ 
		        " leads to an empty store.\n") ;
	       unsat_store cstr store
	     | Range(tbl) ->
	       ( match cstr with
		        EqZ(ne) -> add_eq_to_store store bddt ne
		      | Bv _ -> assert false
		      | Ineq ineq ->
		        let (vn, nac) = normalise ineq in
			     ( match
			         (
			           nac,
			           try
				          (mfind vn tbl)
			           with Not_found ->
				          print_string ("\n" ^ vn ^ 
					                     " not found in the table!\n");
				          StringMap.iter
				            (fun key range ->
				               print_string (
				                 "\n\t" ^ key ^ " " ^
				                 (Polyhedron.range_to_string range)
				               );
				            )
				            tbl;
				          flush stdout;
				          assert false
			         )
			       with
			         (NSupEqI(i), RangeI(min, max)) ->
				      if
				        Num.le_num i min
				      then
				        {var=Range(tbl) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn}
				      else if
				        Num.gt_num i max
				      then
				        {var=Unsat(cstr, store) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn}
				      else (* min < i <= max *)
				        let tbl1, sl1, d1, da =
				          if
				            Num.eq_num i max
				          then
				  (*
Whenever, a variable becomes bounded, we:
- add it to the list of substitutions
- remove it from the store.
- check if delayed cstr should be awaked
once this new subst has been applied
*)
				            let s = make_subst vn (I max) in
				            let d' =
					           (List.map
					              (Constraint.apply_subst_ineq s)
					              d
					           )
				            in
				            let (d_awake, d_delay) =
					           List.partition
					             (fun ineq ->
					                Constraint.dimension_ineq ineq <= 1)
					             d'
				            in
					         (StringMap.remove vn tbl,
					          s::sl,
					          d_delay,
					          d_awake)
				          else
				            (
					           StringMap.add vn (RangeI(i, max)) tbl,
					           sl,
					           d,
					           []
				            )
				        in
				        (* Applying the waked constraints *)
				        List.fold_left
				          (fun acc cstr ->
					          if debug_store2 then (
					            print_string "\n <== awake ";
					            print_string
					              (Constraint.ineq_to_string cstr);
					            flush stdout
					          );
					          add_constraint acc bddt (Ineq cstr)
				          )
				          { var=Range(tbl1) ; substl=sl1 ; delay=d1 ;
					         untouched = rm uvars vn}
				          da
			       |
			         (NInfEqI(i), RangeI(min, max)) ->
			         if
				        Num.lt_num i min
			         then
				        { var=Unsat(cstr, store) ; substl=sl ; delay=d;
				          untouched = rm uvars vn }
			         else if
				        Num.ge_num i max
			         then
				        { var=Range(tbl) ; substl=sl ; delay=d;
				          untouched = rm uvars vn  }
			         else (* min <= i < max *)
				        let tbl1, sl1, d1, da =
				          if
				            Num.eq_num i min
				          then
				            let s = make_subst vn (I min) in
				            let (d_awake, d_delay) =
				              List.partition
					             (fun ineq ->
					                Constraint.dimension_ineq ineq <= 1)
					             (List.map
					                (Constraint.apply_subst_ineq s)
					                d)
				            in
				            (StringMap.remove vn tbl,
					          s::sl, d_delay, d_awake)
				          else
				            (
				              StringMap.add vn (RangeI(min,i)) tbl, sl,
				              d,
				              []
				            )
				        in
				        (* Applying the waked constraints *)
				        List.fold_left
				          (fun acc cstr ->
					          if debug_store2 then (
					            print_string "\n <== awake ";
					            print_string (Constraint.ineq_to_string cstr);
					            flush stdout
					          );
					          add_constraint acc bddt (Ineq cstr))
				          { var=Range(tbl1) ; substl=sl1 ; delay=d1;
				            untouched = rm uvars vn  }
				          da

			       |  (NSupEqF(f), RangeF(min, max)) ->
				      if
				        f <= min
				      then
				        {var=Range(tbl) ; substl=sl ; delay=d;
				         untouched = rm uvars vn  }
				      else if
				        f > max
				      then
				        {var=Unsat(cstr, store) ; substl=sl ; delay=d;
				         untouched = rm uvars vn  }
				      else (* min < f <= max *)
				        let tbl1, sl1, d1, da =
				          if
				            f = max
				          then
				            let s = make_subst vn (F max) in
				            let (d_awake, d_delay) =
					           List.partition
					             (fun ineq ->
					                Constraint.dimension_ineq ineq <= 1)
					             (List.map
					                (Constraint.apply_subst_ineq s)
					                d)
				            in
					         (StringMap.remove vn tbl,
					          s::sl, d_delay, d_awake)
				          else
				            (
					           StringMap.add vn (RangeF(f, max)) tbl,
					           sl,
					           d,
					           []
				            )
				        in
				        (* Applying the waked constraints *)
				        List.fold_left
				          (fun acc cstr ->
					          if debug_store2 then (
					            print_string "\n <== awake ";
					            print_string (Constraint.ineq_to_string cstr);
					            flush stdout
					          );
					          add_constraint acc bddt (Ineq cstr))
				          { var=Range(tbl1) ; substl=sl1 ; delay=d1;
					         untouched = rm uvars vn  }
				          da

			       |
			         (NInfEqF(f), RangeF(min, max)) ->
			         if
				        f < min
			         then
				        {var=Unsat(cstr, store) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn }
			         else if
				        f >= max
			         then
				        {var=Range(tbl) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn }
			         else (* min <= f < max *)
				        let tbl1, sl1, d1, da =
				          if
				            f = min
				          then
				            let s = make_subst vn (F min) in
				            let (d_awake, d_delay) =
				              List.partition
					             (fun ineq ->
					                Constraint.dimension_ineq ineq <= 1)
					             (List.map
					                (Constraint.apply_subst_ineq s)
					                d)
				            in
				            (StringMap.remove vn tbl,
					          s::sl, d_delay, d_awake)
				          else
				            (
				              StringMap.add vn (RangeF(min, f)) tbl,
				              sl,
				              d,
				              []
				            )
				        in
				        (* Applying the waked constraints *)
				        List.fold_left
				          (fun acc cstr ->
					          if debug_store2 then (
					            print_string "\n <== awake ";
					            print_string (Constraint.ineq_to_string cstr);
					            flush stdout
					          );
					          add_constraint acc bddt (Ineq cstr))
				          { var=Range(tbl1) ; substl=sl1 ; delay=d1;
				            untouched = rm uvars vn  }
				          da

			       |  (NSupF(f), RangeF(min, max)) ->
				      if
				        f < min
				      then
				        {var=Range(tbl) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn }
				      else if
				        f >= max
				      then
				        {var=Unsat(cstr, store) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn }
				      else (* min <= f < max *)
				        let (tbl1, sl1, d1, da) =
				          if
				            (f +. eps) = max
				          then
				            let s = make_subst vn (F max) in
				            let (d_awake, d_delay) =
					           List.partition
					             (fun ineq ->
					                Constraint.dimension_ineq ineq <= 1)
					             (List.map
					                (Constraint.apply_subst_ineq s)
					                d)
				            in
					         (StringMap.remove vn tbl,
					          s::sl, d_delay, d_awake)
				          else
				            (
					           StringMap.add vn (RangeF(f+.eps, max)) tbl,
					           sl,
					           d,
					           []
				            )
				        in
				        (* Applying the waked constraints *)
				        List.fold_left
				          (fun acc cstr ->
					          if debug_store2 then (
					            print_string "\n <== awake ";
					            print_string (Constraint.ineq_to_string cstr);
					            flush stdout
					          );
					          add_constraint acc bddt (Ineq cstr))
				          { var=Range(tbl1) ; substl=sl1 ; delay=d1 ;
					         untouched = rm uvars vn }
				          da

			       |
			         (NInfF(f), RangeF(min, max)) ->
			         if
				        f <= min
			         then
				        {var=Unsat(cstr, store) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn }
			         else if
				        f > max
			         then
				        {var=Range(tbl) ; substl=sl ; delay=d ;
				         untouched = rm uvars vn }
			         else (* min < f <= max *)
				        let tbl1, sl1, d1, da =
				          if
				            (f -. eps) = min
				          then
				            let s = make_subst vn (F min) in
				            let (d_awake, d_delay) =
				              List.partition
					             (fun ineq ->
					                Constraint.dimension_ineq ineq <= 1)
					             (List.map
					                (Constraint.apply_subst_ineq s)
					                d)
				            in
				            (StringMap.remove vn tbl,
					          s::sl, d_delay, d_awake)
				          else
				            (
				              StringMap.add vn (RangeF(min, f-.eps)) tbl,
				              sl,
				              d,
				              []
				            )
				        in
				        (* Applying the waked constraints *)
				        List.fold_left
				          (fun acc cstr ->
					          if debug_store2 then (
					            print_string "\n <== awake ";
					            print_string (Constraint.ineq_to_string cstr);
					            flush stdout
					          );
					          add_constraint acc bddt (Ineq cstr)
				          )
				          { var=Range(tbl1) ; substl=sl1 ; delay=d1 ;
				            untouched = rm uvars vn }
				          da

			       | _ -> assert false
			     )
	       )
    in
    if debug_store then (
	   print_string "\n>>>> The Store after:\n";
	   print_string (to_string  res);
	   print_string "\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< \n";
	   flush stdout
    );
    res

and (add_eq_to_store : t -> Formula_to_bdd.t ->  Ne.t -> t) =
  fun store bddt ne ->
    (* [add_eq_to_store s e] returns the store [s] with the numeric
        constraint [EqZ(e)] added.
    *)
    if debug_store2 then (
      print_string ("add_eq_to_store " ^ (Ne.to_string ne) ^ " \n");
      print_string (to_string  store);
      flush stdout;
    );
    let dim = Ne.dimension ne in
    if
	   dim = 0
    then
	   match (Ne.find "" ne) with
	   | Some x -> if Value.num_eq_zero x then store else 
	       unsat_store (EqZ ne) store
	   | None -> store
    else
	   (* dim > 0 *)

      match Ne.split ne with 
      | Ne.No_solution -> unsat_store (EqZ ne) store
      | Ne.Dont_know -> store
      | Ne.Split(vn, coef, ne_rest) -> 

	     (*
if ne = "a0 + a1x1 + a2x2 + ...", then
- vn,coef = ("x1", a1) 
                (or any other indexes except the constant one !!!), 
                - and ne_rest = "a0 + a2x2 + ..."
*)
	     let coef_neg = Value.neg coef in
	     let store1 = 
	 (*
 Propagates the bounds of vn (min and max) to what 
 (-a0).vn will be substituted to. 
*)
		    match store.var with
		      Unsat(_,_) -> assert false
		    | Range(tbl) ->
		      let range_vn = 
			     try mfind vn tbl
			     with Not_found -> 
			       print_string (vn ^ " not found\n store= ");
			       print_string (to_string store);
			       assert false
		      in
		      let (vn_min, vn_max) =
			     match range_vn with
			     | RangeI(min, max) -> I min, I max
			     | RangeF(min, max) -> F min, F max
		      in
		      let (vn_min1, vn_max1) = 
			     if Value.num_supeq_zero coef_neg then
			       (Value.mult_num coef_neg vn_min,
			        Value.mult_num coef_neg vn_max)
			     else
			       (Value.mult_num coef_neg vn_max,
			        Value.mult_num coef_neg vn_min)
		      in
		      let cstr_min = Ineq (GeqZ(Ne.diff ne_rest (Ne.make "" vn_min1)))
		      and cstr_max = Ineq (GeqZ(Ne.diff (Ne.make "" vn_max1) ne_rest)) in
		      let new_store_var =
			     (* We do not need the bounds of [vn] anymore *)
			     Range(StringMap.remove vn tbl)
		      in
		      let storea =
			     {
			       var = new_store_var;
			       substl = store.substl;
			       delay = store.delay ;
			       untouched = rm store.untouched vn
			     }
		      in
		      let storeb = add_constraint storea bddt cstr_min in
		      let storec = add_constraint storeb bddt cstr_max in
			   storec
	     in

	     (* The new substitution *)
	     let s = ((vn, coef_neg), ne_rest) in

	     (*  [vn] elimination in the delayed constraints *)
	     let d = store1.delay in
	     let d2 = List.map (Constraint.apply_subst_ineq s) d in
		  (* Some delayed constraints may have been awaken by this
		     substitution (awake = become of dim 1). *)
	     let (waked, d3) = List.partition 
		      (fun cstr -> Constraint.dimension_ineq cstr <= 1) d2
	     in
	     let store2 =
		    List.fold_left
		      (fun acc cstr ->
		         if debug_store2 then (
		           print_string (
			          "\n <== awake "^ (Constraint.ineq_to_string cstr));
		           flush stdout
		         );
		         add_constraint acc bddt (Ineq cstr))
		      { 
		        var = store1.var; 
		        substl = s::(store1.substl); 
		        delay = d3 ;
		        untouched = store1.untouched 
		      }
		      waked
	     in
		  store2
		

(*******************************************************************************)
(* exported *)
let (is_store_satisfiable : int -> t -> bool) =
  fun verb store ->
    match store.var with 
	     Unsat(cstr,store) -> 
	       if verb >= 2 then
	         (
	           print_string (
		          "# adding the constraint " ^ (Constraint.to_string cstr) ^
		            " led to an empty set of solution");
		        print_string (" when added to the store " ^ (to_string store));
	           print_string "\n";
	           flush stdout
	         ); 
	       false
      | _ -> true
