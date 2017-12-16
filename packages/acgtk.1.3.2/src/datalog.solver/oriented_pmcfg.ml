open Pmcfg
open Datalog_signature
open String_map
open Int_map
open Program


module Oriented_pmcfg = 
  struct open PMCFG

  type arg_order = Arg_order of (int list)
  type var_order = Var_order of (int list)
  type ordered_cat = Ord_cat of int * arg_order


  module Ordered_ordered_cat = 
    struct
      type t= ordered_cat
      let compare ord_cat1 ord_cat2 = 
         (match (ord_cat1,ord_cat2) with
	  (Ord_cat(k1,arg_ord_1),Ord_cat(k2,arg_ord_2)) ->
	    if k1=k2
	    then
	      Pervasives.compare arg_ord_1 arg_ord_2
	    else 
	      (if k1>k2
	      then 1
		else -1
	      )
      )
  end
    
  module Ordered_cat_map = Map.Make(Ordered_ordered_cat)
  module Ordered_cat_set = Set.Make(Ordered_ordered_cat)

  type 'a transformation_info = 
      {map_ord_cat_to_id: 'a Ordered_cat_map.t ; 
       ordered_cat_to_treat:Ordered_cat_set.t ;  
       old_sign : Datalog_signature.signature;
       new_sign : Datalog_signature.signature}

  let get_category ord_cat sign= 
    (match ord_cat with
	Ord_cat(k,Arg_order arg_ord) ->
	  let (rank,name) = Datalog_signature.get_predicate k sign in
          let arity = List.length arg_ord in
	  let is_order_id arg_ord=
	    let rec check_order_id l n =
	      (match l with 
		  [] -> true
		| q::tl ->
		    n=q  && (check_order_id tl (n+1))
	      )
	    in
              check_order_id arg_ord 0
	  in
	    if (arity=rank) && (is_order_id arg_ord)
	    then (rank,name)
	    else 
	     (arity,name^"_"^(String.concat "" (List.map string_of_int arg_ord)))
    )

  let is_trans_info_empty trans_info = 
    Ordered_cat_set.is_empty (trans_info.ordered_cat_to_treat)

  let get_next_ord_cat trans_info= 
    let ord_cat = Ordered_cat_set.min_elt trans_info.ordered_cat_to_treat in
    let (_,name) = get_category ord_cat trans_info.old_sign in
    let _ = print_string ("Treating: "^name^".\n") in
      (ord_cat,
      {map_ord_cat_to_id = trans_info.map_ord_cat_to_id;
       ordered_cat_to_treat = 
	  Ordered_cat_set.remove
	    ord_cat
	    trans_info.ordered_cat_to_treat;
       old_sign = trans_info.old_sign;
       new_sign = trans_info.new_sign
      }
      )
        

  let get_initial_trans_info ord_cat sign = 
    let (rank,name) = get_category ord_cat sign in
    let new_sign = Datalog_signature.add_pred rank name Datalog_signature.empty in
      {map_ord_cat_to_id = Ordered_cat_map.add ord_cat 0 Ordered_cat_map.empty; 
       ordered_cat_to_treat = Ordered_cat_set.singleton ord_cat ;
       old_sign=sign;
       new_sign= new_sign
      }

  let get_id_ord_cat trans_info ord_cat = 
    (try
	let id = Ordered_cat_map.find ord_cat trans_info.map_ord_cat_to_id in
	  (id,trans_info)
      with Not_found ->
	let id = Datalog_signature.fresh trans_info.new_sign in
	let (rank,name) = get_category ord_cat trans_info.old_sign in
	let new_sign = Datalog_signature.add_pred rank name trans_info.new_sign in
	let map = Ordered_cat_map.add ord_cat id trans_info.map_ord_cat_to_id in
	let to_treat = Ordered_cat_set.add ord_cat trans_info.ordered_cat_to_treat in
	  (id,
	  {map_ord_cat_to_id = map; 
	   ordered_cat_to_treat = to_treat;
	   old_sign = trans_info.old_sign;
	   new_sign = new_sign})
    )

  let get_rules_defining_ordered_cat ord_cat grammar = 
    match ord_cat with
	Ord_cat(cat,_) -> get_rules_defining_cat cat grammar

  let get_var_order ordered_cat lhs_cat =
    (match lhs_cat with
	Lhs_cat(_,arg_list) ->
	  let rec compute_var_list arg var_list= 
	    (match arg with
		[] -> var_list
	      | (Var n)::arg -> 
                  if List.mem n var_list
                  then compute_var_list arg var_list
                  else compute_var_list arg (n::var_list)
	      | _::arg -> compute_var_list arg var_list
	    )
	  in
	  let rec compute_var_order arg_order var_order= 
	    (match arg_order with
		[] -> Var_order (List.rev var_order)
	      | n::arg_order ->
		  let nth_arg = List.nth arg_list n in
		    compute_var_order arg_order (compute_var_list nth_arg var_order)
	    )
	  in
	    (match ordered_cat with
		Ord_cat(_,Arg_order arg_order) ->
		  compute_var_order arg_order []
	    )
    )

  let get_ordered_cat var_order rhs_cat =
    (match rhs_cat with
	Rhs_cat(cat,vars) ->
	  (match var_order with
	      Var_order var_order ->
		let var_order = List.filter (function var -> List.mem var vars) var_order in
		let get_var_position var=
		  let rec compute_var_position vars n =
		    (match vars with 
			[] -> failwith 
			  ("get_ordered_cat: get_var_position: variable "^(string_of_int n)^" not found")
		      | var0::vars ->
			  if var=var0
			  then n
			  else compute_var_position vars (n+1)
		    )
		  in
		    compute_var_position vars 0
		in
		let positioned_vars = List.map get_var_position var_order
                in
		  Ord_cat(cat, Arg_order positioned_vars)
	  )
    )

  let build_rhs id ord_cat rhs_cat =
    (match rhs_cat with
	Rhs_cat (p,vars)->
	  let rec compute_vars_order remaining_order rev_vars= 
	    (match remaining_order with
		[] -> List.rev rev_vars
	      | n::tl -> compute_vars_order tl ((List.nth vars n)::rev_vars))
	  in
	    (match ord_cat with
		Ord_cat(n,Arg_order ord_vars) ->
		  Rhs_cat(id,compute_vars_order ord_vars [])
	    )	
    )

  let build_lhs id ord_cat lhs_cat = 
    (match lhs_cat with
	Lhs_cat (p,args)->
	  let rec compute_vars_order remaining_order rev_args= 
	    (match remaining_order with
		[] -> List.rev rev_args
	      | n::tl -> compute_vars_order tl ((List.nth args n)::rev_args))
	  in
	    (match ord_cat with
		Ord_cat(n,Arg_order ord_vars) ->
		  Lhs_cat(id,compute_vars_order ord_vars [])
	    )	
    )

  let collect_ordered_cat_from_rule 
      ordered_cat trans_info grule =
    (match grule with
	R(lhs,rhs) -> 
	  let var_order = get_var_order ordered_cat lhs in
          let (rhs,trans_info) = 
	    List.fold_left
	      (function (new_rhs,trans_info) ->
		function rhs_cat ->
		  let Ord_cat(n,Arg_order ord_vars) as ord_cat = 
                    get_ordered_cat var_order rhs_cat 
                  in
                    if ord_vars=[]
                    then (new_rhs, trans_info)
                    else
		      let (id_cat,trans_info) =  get_id_ord_cat trans_info ord_cat in
		        (new_rhs@[build_rhs id_cat ord_cat rhs_cat], trans_info)
	      )
	      ([],trans_info)
	      rhs
	  in 
	  let (id,_) = get_id_ord_cat trans_info ordered_cat in
	  let lhs = build_lhs id ordered_cat lhs in
	    (R(lhs,rhs), trans_info)
    )

  let orient_grammar grammar = 
    (match grammar with 
	Grammar(sign,rules,init) ->
	  let rec compute_new_grammar trans_info rules= 
	    (if is_trans_info_empty trans_info
	    then Grammar (trans_info.new_sign, rules,init)
	      else
		let (ord_cat,trans_info) = get_next_ord_cat trans_info in
		let ord_cat_rules = get_rules_defining_ordered_cat ord_cat grammar in
		let (new_rules,trans_info) = 
		  List.fold_left
		    (function (n_rules, trans_info) ->
		      function grule ->
			let (grule,trans_info) = 
                          collect_ordered_cat_from_rule ord_cat trans_info grule 
                        in
			  (n_rules@[grule],trans_info)
		    )
		    ([],trans_info)
		    ord_cat_rules
		in
		  compute_new_grammar trans_info (rules@new_rules)
	    )
	  in
	  let ord_cat = Ord_cat (init,Arg_order[0]) in
	    compute_new_grammar (get_initial_trans_info ord_cat sign) []
    )
end
