open Datalog_signature
open String_map
open Int_map
open Program


module PMCFG =
struct
  type argument = Var of int | Val of string
  type rhs_cat = Rhs_cat of int*(int list)
  type lhs_cat = Lhs_cat of int*((argument list) list)
  type grule = R of lhs_cat*(rhs_cat list)
  type grammar = Grammar of (Datalog_signature.signature*(grule list)*int)

  let is_rule_defining_cat cat grule =
    match grule with 
	R(Lhs_cat(cat0,_),_) -> cat =cat0

  let get_rules_defining_cat cat grammar = 
    match grammar with
	Grammar (_,rules,_) -> 
	  List.filter (is_rule_defining_cat cat) rules
	
       
  let program_of_pmcfg_gen grammar is_naive = 
    match grammar with
	Grammar (sign,rules,init) -> 
	  let new_sign = 
	    (match sign with
		Datalog_signature.S(l,_,_) ->
		  Datalog_signature.S(
		    List.map 
		      (function (rank,name) ->(2*rank,name))
		      l
                      ,
                    None
                      ,
                    None
		  )
	    )
	  in
          let (rg_eq,new_sign) = Datalog_signature.add_pred_get_id 4 "eq" new_sign in
          let (eq,new_sign) = Datalog_signature.add_eq_get_id new_sign in
          let (neq,new_sign) = Datalog_signature.add_neq_get_id new_sign in
	  let string_predicates = String_map.empty in
	  let rec get_ranges_and_ext_pred_of_arg j l ranges ext_pred string_predicates sign=
	    (match l with
		[] -> (j,ranges,ext_pred,string_predicates,sign)
	      | (Var v)::tl -> 
                  (try 
                      let (fst,rg) = Int_map.find v ranges in
                      let ranges = Int_map.add v (fst,(j,j+1)::rg) ranges in
		        get_ranges_and_ext_pred_of_arg (j+1) tl ranges ext_pred string_predicates sign
                    with
                        Not_found ->
                          let ranges = Int_map.add v ((j,j+1),[]) ranges in
		            get_ranges_and_ext_pred_of_arg (j+1) tl ranges ext_pred string_predicates sign
                  )
                    
	      | (Val a)::tl ->
                  if a = ""
                  then 
                    let a_pred = Program.Pred (eq,[j;j+1]) in
		      get_ranges_and_ext_pred_of_arg (j+1) tl ranges (a_pred::ext_pred) string_predicates sign
                  else
		    (try 
		        let id = String_map.find a string_predicates in
		        let a_pred = Program.Pred (id,[j;j+1]) in
			  get_ranges_and_ext_pred_of_arg (j+1) tl ranges (a_pred::ext_pred) string_predicates sign
		      with Not_found ->
		        let id = Datalog_signature.fresh sign in
		        let string_predicates = String_map.add a id string_predicates in
		        let sign = Datalog_signature.add_pred 2 a sign in
		        let a_pred = Program.Pred (id,[j;j+1]) in
			  get_ranges_and_ext_pred_of_arg (j+1) tl ranges (a_pred::ext_pred) string_predicates sign
		    )

	    )
	  in
	  let pred_of_rhs ranges rhs_cat= 
	    (match rhs_cat with
		Rhs_cat(id,vars) ->
		  Program.Pred
		    (id,
		    List.fold_left
		      (function l ->
			function v ->
			  let ((i,j),_) = Int_map.find v ranges in
			    l@[i;j]
		      )
		      []
		      vars
		    )
	    )
	  in
          let eq_pred ranges = 
            Int_map.fold
              (function i ->
                function ((i,j),eq_list) ->
                  function eq_pred_list ->
                    eq_pred_list @
                      (List.rev_map 
                          (function (k,l) ->
                            Program.Pred(rg_eq,[i;j;k;l])
                          )
                          eq_list
                      )
              )
              ranges
              []
          in
	  let clause_of_rule grule string_predicates sign = 
	    (match grule with
		R(Lhs_cat(id,args),rhs) ->
		  let (vars,i,ranges,ext_pred,string_predicates,sign) =
		    List.fold_left
		      (
			function (vars,i,ranges,ext_pred,string_predicates,sign) ->
			  function arg ->
			    let (j,ranges,ext_pred,string_predicates,sign) = 
			      get_ranges_and_ext_pred_of_arg i arg ranges ext_pred string_predicates sign
			    in
			      (vars@[i;j],(j+1),ranges,ext_pred,string_predicates,sign)
		      )
		      ([],0,Int_map.empty,[],string_predicates,sign)
		      args
		  in
		    (Program.Cl(
		      Program.Pred(id,vars), (List.rev ext_pred)@
		        (List.map (pred_of_rhs ranges) rhs)@(eq_pred ranges)),
		    string_predicates,
		    sign
		    )
	    )
	  in
          let get_eq_clauses string_predicates = 
            (Program.Cl(Program.Pred(rg_eq,[0;1;2;3]),[Program.Pred(eq,[0;1]); Program.Pred(eq,[2;3])]))
            ::
              (String_map.fold
                  (function s ->
                    function id ->
                      function rules ->
                        (if is_naive
                        then
                          (Program.Cl(Program.Pred(rg_eq,[0;2;3;5]),
                                     [Program.Pred(id,[0;1]);
                                      Program.Pred(id,[3;4]);
                                      Program.Pred(rg_eq,[1;2;4;5])
                                     ]
                          ))
                        else
                          (Program.Cl(Program.Pred(rg_eq,[0;2;3;5]),
                                     [Program.Pred(neq,[0;2]);
                                      Program.Pred(id,[0;1]);
                                      Program.Pred(id,[3;4]);
                                      Program.Pred(rg_eq,[1;2;4;5])
                                     ]
                          )))
                        ::
                          rules
                  )
                  string_predicates
                  []
              )
          in
	  let (clauses,string_predicates,new_sign) = 
	    List.fold_left
	      (function (clauses,string_predicates,sign) ->
		function grule ->
		  let (clause,string_predicates,sign) = 
		    clause_of_rule grule string_predicates sign
		  in
		    (clause::clauses,string_predicates,sign)
	      )
	      ([],string_predicates,new_sign)
	      rules
	  in
          let eq_clauses = get_eq_clauses string_predicates in
	    Program.Prog (new_sign,(clauses @ eq_clauses))

  let program_of_pmcfg grammar = program_of_pmcfg_gen grammar false 
  let naive_program_of_pmcfg grammar = program_of_pmcfg_gen grammar true

end
