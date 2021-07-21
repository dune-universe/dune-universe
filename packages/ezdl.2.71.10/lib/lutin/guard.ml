(*

Manage constraints (i.e. guards) during automaton exploration.

Partial eval is from CoAlgExp to CoAlgExp.
Maybe not a good idea: how to directly use structures suitable for the solver ?

*)

open Value
open CoAlgExp
open CkTypeEff
open LutinUtils

let _dbg = Verbose.get_flag "Guard"

(* a guard is splitted into:
	- a Value.OfIdent.t corresponding to the set of "var = val" constraints
	- a list of remaining constaints

*)
type t = {
	idies : Value.OfIdent.t ;
	others : CoAlgExp.t list ;
   src : CoTraceExp.src_info list 
}

let empty = { idies = Value.OfIdent.empty ; others = [] ; src = [] }

type store = { curs : Value.OfIdent.t; pres: Value.OfIdent.t }

let empty_store = { curs=Value.OfIdent.empty; pres=Value.OfIdent.empty }

let get_store c p = { curs=c; pres=p}

type unalias = CoIdent.t ->  CoAlgExp.t

exception No_unalias
let no_unalias = fun _ -> ( raise No_unalias )

(* SEMANTICS RULES FOR PARTIAL EVAL *)
let is_true e = (e.ae_val = AE_true)
let is_false e = (e.ae_val = AE_false)

let dbg = Verbose.get_flag "Guard"

exception No_simp

let val2exp = function
| B true -> CoAlgExp.of_true	
| B false -> CoAlgExp.of_false	
| N (I i) -> CoAlgExp.of_ival (Util.int_of_num i)
| N (F f) -> CoAlgExp.of_rval f


let to_exp_list g = (
	let idy2exp (v,x) = (
		let ex = val2exp x in
		let ev = CoAlgExp.of_support v (CoAlgExp.get_type ex) false in
		CoAlgExp.of_eq ev ex
	) in
	(List.map idy2exp (Value.OfIdent.content g.idies))@(g.others)
)

let to_string g = (
	let cl = List.map CoAlgExp.lus_dumps (to_exp_list g) in
	String.concat " and " cl
)

let dumpf os g = (
	Printf.fprintf os "%s" (to_string g)
)

let rec simp_call id ty ops = try (
  match (id, ops) with	
	   (* bool *)
	 |  ("not", [op]) ->
		   if (is_true op) then of_false
		   else if (is_false op) then of_true
		   else raise No_simp
	 |  ("and", [o1;o2]) -> (
		   if((is_false o1) || (is_true o2)) then o1
		   else if((is_false o2) || (is_true o1)) then o2
		   else raise No_simp
	    )
	 |  ("or", [o1;o2]) -> (
		   if((is_false o1) || (is_true o2)) then o2
		   else if((is_false o2) || (is_true o1)) then o1
		   else raise No_simp
	    )
	 |  ("xor", [o1;o2]) -> (
		   simp_call "neq" boolean [o1;o2]
	    )
	 |  ("impl", [o1;o2]) -> (
		   let o1' = simp_call "not" boolean [o1] in
		     simp_call "or" boolean [o1'; o2]
	    )
	      (* comp *)
	 | ("lt", [o1;o2]) -> (
		  match (o1.ae_val, o2.ae_val) with
		    |	( AE_ival i1,  AE_ival i2) -> of_bval (i1 < i2)
		    |	( AE_rval r1,  AE_rval r2) -> of_bval (r1 < r2)
		    |  _ -> raise No_simp
	   )
	 | ("lte", [o1;o2]) -> (
		  match (o1.ae_val, o2.ae_val) with
		    |	( AE_ival i1,  AE_ival i2) -> of_bval (i1 <= i2)
		    |	( AE_rval r1,  AE_rval r2) -> of_bval (r1 <= r2)
		    |  _ -> raise No_simp
	   )
	 | ("gt", [o1;o2]) -> (
		  match (o1.ae_val, o2.ae_val) with
		    |	( AE_ival i1,  AE_ival i2) -> of_bval (i1 > i2)
		    |	( AE_rval r1,  AE_rval r2) -> of_bval (r1 > r2)
		    |  _ -> raise No_simp
	   )
	 | ("gte", [o1;o2]) -> (
		  match (o1.ae_val, o2.ae_val) with
		    |	( AE_ival i1,  AE_ival i2) -> of_bval (i1 >= i2)
		    |	( AE_rval r1,  AE_rval r2) -> of_bval (r1 >= r2)
		    |  _ -> raise No_simp
	   )
	 | ("eq", [o1;o2]) -> (
		  match (o1.ae_val, o2.ae_val) with
		    |	( AE_ival i1,  AE_ival i2) -> of_bval (i1 = i2)
		    |	( AE_rval r1,  AE_rval r2) -> of_bval (r1 = r2)
		    |	( AE_true, _) -> o2 
		    |	( _, AE_true) -> o1 
		    |	( AE_false, _) -> simp_call "not" boolean [o2]
		    |	( _, AE_false) -> simp_call "not" boolean [o1] 
		    |  _ -> raise No_simp
	   )
	 | ("neq", [o1;o2]) -> (
		  match (o1.ae_val, o2.ae_val) with
		    |	( AE_ival i1,  AE_ival i2) -> of_bval (i1 <> i2)
		    |	( AE_rval r1,  AE_rval r2) -> of_bval (r1 <> r2)
		    |	( AE_false, _) -> o2 
		    |	( _, AE_false) -> o1 
		    |	( AE_true, _) -> simp_call "not" boolean [o2]
		    |	( _, AE_true) -> simp_call "not" boolean [o1] 
		    |  _ -> raise No_simp
	   )
	     (* int/real *)
	 |  ("uminus", [o]) -> (
		   match o.ae_val with
		     |  AE_ival i -> of_ival (-i)
		     |  AE_rval r -> of_rval (-. r)
		     |  _ -> raise No_simp
	    )
	 |  ("plus", [o1;o2]) -> (
		   match (o1.ae_val, o2.ae_val) with
		     |	( AE_ival i1,  AE_ival i2) -> of_ival (i1 + i2)
		     |	( AE_rval r1,  AE_rval r2) -> of_rval (r1 +. r2)
		     |  _ -> raise No_simp
	    )
	 |  ("minus", [o1;o2]) -> (
		   match (o1.ae_val, o2.ae_val) with
		     |	( AE_ival i1,  AE_ival i2) -> of_ival (i1 - i2)
		     |	( AE_rval r1,  AE_rval r2) -> of_rval (r1 -. r2)
		     |  _ -> raise No_simp
	    )
	 |  ("times", [o1;o2]) -> (
		   match (o1.ae_val, o2.ae_val) with
		     |	( AE_ival i1,  AE_ival i2) -> of_ival (i1 * i2)
		     |	( AE_rval r1,  AE_rval r2) -> of_rval (r1 *. r2)
		     |  _ -> raise No_simp
	    )
	 |  ("slash", [o1;o2]) -> (
		   match (o1.ae_val, o2.ae_val) with
		     |	( AE_ival i1,  AE_ival i2) -> of_ival (i1 / i2)
		     |	( AE_rval r1,  AE_rval r2) -> of_rval (r1 /. r2)
		     |  _ -> raise No_simp
	    )
	      (* int *)
	 |  ("mod", [o1;o2]) -> (
		   match (o1.ae_val, o2.ae_val) with
		     |	( AE_ival i1,  AE_ival i2) -> of_ival (i1 mod i2)
		     |  _ -> raise No_simp
	    )
	 |  ("div", [o1;o2]) -> (
		   match (o1.ae_val, o2.ae_val) with
		     |	( AE_ival i1,  AE_ival i2) -> of_ival (i1 / i2)
		     |  _ -> raise No_simp
	    )
	      (* int/weight *)
	 | ("interval_continue", [o1;o2;o3]) -> (
		  match (o1.ae_val,o2.ae_val,o3.ae_val) with
		    | (AE_ival a1, AE_ival a2, AE_ival a3) -> of_ival (interval_continue a1 a2 a3)
		    | _ -> assert false 
	   )
	 | ("interval_stop", [o1;o2;o3]) -> (
		  match (o1.ae_val,o2.ae_val,o3.ae_val) with
		    | (AE_ival a1, AE_ival a2, AE_ival a3) -> of_ival (interval_stop a1 a2 a3)
		    | _ -> assert false 
	   )
	 | ("gauss_continue", [o1;o2;o3]) -> (
		  match (o1.ae_val,o2.ae_val,o3.ae_val) with
		    | (AE_ival a1, AE_ival a2, AE_ival a3) -> of_ival (gauss_continue a1 a2 a3)
		    | _ -> assert false 
	   )
	 | ("gauss_stop", [o1;o2;o3]) -> (
		  match (o1.ae_val,o2.ae_val,o3.ae_val) with
		    | (AE_ival a1, AE_ival a2, AE_ival a3) -> of_ival (gauss_stop a1 a2 a3)
		    | _ -> assert false 
	   )
	     (* any *)
	 |  ("ite", [o1;o2;o3]) -> (
		   let nego1 = simp_call "not" boolean [o1] in
		     match (o1.ae_val,o2.ae_val,o3.ae_val) with
		       | (AE_true ,    _    ,    _    ) -> o2
		       | (AE_false,    _    ,    _    ) -> o3
		       | (   _    , AE_true ,    _    ) -> simp_call "or" boolean [o1;o3]
		       | (   _    , AE_false,    _    ) -> simp_call "and" boolean [nego1;o3]
		       | (   _    ,    _    , AE_false) -> simp_call "and" boolean [o1;o2]
		       | (   _    ,    _    , AE_true ) -> simp_call "or" boolean [nego1;o2]
		       |  _ -> raise No_simp
	    )
	      (* noting to do ... *)
	 | _ -> raise No_simp
) with No_simp -> CoAlgExp.of_call id ty ops 

(* PARTIAL EVAL *)
let simplify_exp unalias ctx e = (
	(* util *)
	let rec reval e = (
		let nat = e.CoAlgExp.ae_val in
		let ty  = e.CoAlgExp.ae_type in
   	match nat with
		(* nothing to do *)
		|  AE_true
		|  AE_false
		|  AE_ival _
		|  AE_rval _ -> e
		(* string to val *)
		|  AE_iconst str -> CoAlgExp.of_ival (int_of_string str)
		|  AE_rconst str -> CoAlgExp.of_rval (float_of_string str)
		(* maybe in curs *)
		|  AE_support id -> (
Verbose.put ~flag:dbg "Guard::reval (AE_support \"%s\")\n" id ;
			let res = try (
				let v = Value.OfIdent.get ctx.curs id in
				val2exp v
			) with Not_found -> e
			in  res
		)
		(* maybe in pres *)
		|  AE_pre id -> (
			try (
				let v = Value.OfIdent.get ctx.pres id in
(* Verbose.put ~flag:dbg "Guard::reval (AE_pre \"%s\") -> %s\n" id (Value.to_string v); *)
				val2exp v
			) with Not_found -> e
		)
		(* *)
		|  AE_call (id, args) -> (
			let args' = List.map reval args in
			simp_call id ty args'
		)
		|  AE_external_call (d, ei, prof, args) -> (
			let args' = List.map reval args in
			CoAlgExp.of_external_call d ei prof ty args'	
		)
		(* ??? *)
		|  AE_const _str -> (
			assert false
		)
		|  AE_alias id -> (
			try (
				let e' = unalias id
				in reval e' 
			) with No_unalias -> e
		)
	) in
	let res = reval e in

(* Verbose.put ~flag:dbg "Guard::eval %s\n  curs:%s  pres:%s\n  gives: %s\n"
	(CoAlgExp.lus_dumps e)
	(Value.OfIdent.to_string "" ctx.curs)
	(Value.OfIdent.to_string "" ctx.pres)
	(CoAlgExp.lus_dumps res)
	;
*)
	res

)

(* CoAlgExp to Value.t within a context
	Not_constant if not evaluable
*)


exception Not_constant of CoAlgExp.t

let value_of_algexp unalias ctx e = (
	let e' = simplify_exp unalias ctx e in
	match e'.CoAlgExp.ae_val with
	|  CoAlgExp.AE_true -> Value.B true
	|  CoAlgExp.AE_false -> Value.B false
	|  CoAlgExp.AE_ival i -> Value.N (Value.I (Num.num_of_int i))
	|  CoAlgExp.AE_rval r -> Value.N (Value.F r)
	|  _ -> raise (Not_constant e')
)

(* util: splits a formula into a (conjunction) list *)
let rec split_conj e = (
	let nat = e.CoAlgExp.ae_val in
	match nat with
	|  AE_call ("and", [o1;o2]) -> (
		(split_conj o1) @ (split_conj o2)	
	)
	| _ -> [e]
)

exception Unsat
exception Not_an_idy

(* util: add an atomic constraint (i.e. not a "and") to a guard *)
let add_idy g (v,x) = (
	try (
		let oldx = Value.OfIdent.get g v in
		if (oldx = x) then g
		else (
			(* REMARK 1: this branch is normaly never taken
				since expressions are evaluated taking into account "g",
				the Unsat is raised before, during add_atom (see REMARK 2)
			*)
   		Verbose.put ~flag:dbg
				" --> Guard.add_idy: UNSAT GUARD (var %s cannot be both %s and %s)\n"
				v (Value.to_string oldx) (Value.to_string x)
			;
			raise Unsat
		)
	) with Not_found -> (
		Value.OfIdent.add g (v,x)
	)
)
let merge_idies (g1:Value.OfIdent.t) (g2:Value.OfIdent.t) = (
	Value.OfIdent.fold (fun v x g -> add_idy g (v, x)) g1 g2
)
let add_atom si accin e = (
	try (
		(* search for identities *)
		match e.CoAlgExp.ae_val with
		| AE_true -> accin
		| AE_false -> (
			(* REMARK 2: this case appears when a constraint is uncompatible
				with the current set of id(entit)ies 
			*)
   		Verbose.put ~flag:dbg
				" --> Guard.add_atom: UNSAT GUARD\n"
			;
			raise Unsat
		)
		| AE_support v -> (
			(* bool var MUST BE TRUE *)
			{idies=add_idy accin.idies (v,Value.B true); others=accin.others ; src = si::accin.src}
		)
		| AE_call ("not", [o1]) -> (
			match o1.CoAlgExp.ae_val with
			| AE_support v -> (
				(* bool var MUST BE FALSE *)
				{idies=add_idy accin.idies (v,Value.B false); others=accin.others ; src = si::accin.src}
			)
			| _ -> raise Not_an_idy 
		)
		| AE_call ("eq", [o1;o2]) -> (
			match (o1.CoAlgExp.ae_val, o2.CoAlgExp.ae_val) with
			| (AE_support v, AE_ival i)
			| (AE_ival i, AE_support v) -> (
				(* int var MUST BE i *)
				{idies=add_idy accin.idies (v,Value.N (Value.I (Num.num_of_int i))); 
             others=accin.others;
             src = si::accin.src}
			)
			| (AE_support v, AE_rval r)
			| (AE_rval r, AE_support v) -> (
				(* real var MUST BE r *)
				 {idies=add_idy accin.idies (v,Value.N (Value.F r)); 
              others=accin.others ; 
              src = si::accin.src}
			)
			| _ -> raise Not_an_idy
		)
		| _ -> raise Not_an_idy
	) with Not_an_idy -> (
		(* other constraint ... *)
		{idies=accin.idies; others= e::accin.others ; src = si::accin.src }
	)
)

let add ?(unalias=no_unalias) ?(context=None) c g si =
  match context with
  | None -> (
		{idies=g.idies; others= c::g.others ; src = si::g.src }
    )
  | Some ctx -> (

	   Verbose.exe ~flag:dbg (fun _ ->
   	    Verbose.put ~flag:dbg "------------------------------------\n";
   	    Verbose.put ~flag:dbg "-- Add Constraint: %s\n" (CoAlgExp.lus_dumps c);
   	    Verbose.put ~flag:dbg "-- To Guard: %s\n"
			   (String.concat " &\n             " (List.map CoAlgExp.lus_dumps (to_exp_list g)));
   	    ( match context with
              None -> ()
   	      | Some d ->
      	     Verbose.put ~flag:dbg " where curs =\n%s\n" (Value.OfIdent.to_string "" d.curs);
      	     Verbose.put ~flag:dbg " and   pres =\n%s\n" (Value.OfIdent.to_string "" d.pres)
   	    );
	     );

	   (* take both context and current idies in g to simplify c *)
	   let ctx' = { curs = merge_idies g.idies ctx.curs; pres = ctx.pres } in
	   let c' = simplify_exp unalias ctx' c in

	   (* c' does not contain any var from ctx' *)
	   Verbose.exe ~flag:dbg (fun () -> Printf.printf " partial eval gives %s\n"
                                (CoAlgExp.lus_dumps c'));
	   let cl = split_conj c' in
	   let res = List.fold_left (add_atom si) g cl in
      Verbose.exe ~flag:dbg (fun () -> Printf.printf "-- Gives:    %s\n"
		                          (String.concat " &\n             " (List.map CoAlgExp.lus_dumps (to_exp_list res))));
	   res
    )

let of_exp ?(unalias=no_unalias) ?(context=None) c = (
	add ~unalias:unalias ~context:context c empty
)

let merge g1 g2 = (
  (* HERE: check contradictions *)
  let res = {
	 idies = List.fold_left add_idy g2.idies (Value.OfIdent.content g1.idies);
	 others = g1.others@g2.others;
    src = g1.src@g2.src
  } in
  Verbose.exe ~flag:dbg (fun _ ->
   	Verbose.put ~flag:dbg "------------------------------------\n";
   	Verbose.put ~flag:dbg "-- Merge G1: %s\n"
		  (String.concat " &\n             " (List.map CoAlgExp.lus_dumps (to_exp_list g1)));
   	Verbose.put ~flag:dbg "-- And   G2: %s\n"
		  (String.concat " &\n             " (List.map CoAlgExp.lus_dumps (to_exp_list g2)));
   	Verbose.put ~flag:dbg "-- Gives:    %s\n"
		  (String.concat " &\n             " (List.map CoAlgExp.lus_dumps (to_exp_list res)));
	 );
  res
)

(* HERE: really necessary ??? *)
(* let simplify ctx x = x  *)
(* List.map (simplify_exp ctx) x *)
