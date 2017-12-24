open Pmcfg
open Datalog_signature
open String_map
open Int_map
open Program

module PMCFG_to_datalog = 
struct
  open PMCFG

  module Ordered_int_pair =
  struct
    type t=int*int
    let compare p1 p2 = Pervasives.compare p1 p2
  end

  module Pair_map = Map.Make(Ordered_int_pair)

  let gen_eq_name pred k sign =
    "eq_"^(Datalog_signature.get_name pred sign)^"_"^(string_of_int k)

  let get_program (Grammar(sign, rules, init) as grammar) =
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
    let (eq,new_sign) = Datalog_signature.add_eq_get_id new_sign in
    let string_predicates = String_map.empty in
    let rec get_ranges_and_ext_pred_of_arg j l ranges ext_pred string_predicates sign=
      (match l with
	  [] -> (j,ranges,ext_pred,string_predicates,sign)
	| (Var v)::tl -> 
            (try 
                let (fst,rg) = Int_map.find v ranges in
                let ranges = Int_map.add v (fst,(j,j+1)::rg) ranges in
		  get_ranges_and_ext_pred_of_arg (j+1) tl ranges ext_pred
                    string_predicates sign
              with
                  Not_found ->
                    let ranges = Int_map.add v ((j,j+1),[]) ranges in
		      get_ranges_and_ext_pred_of_arg (j+1) tl ranges ext_pred
                        string_predicates sign
            )
              
	| (Val a)::tl ->
	    (try 
		let id = String_map.find a string_predicates in
		let a_pred = Program.Pred (id,[j;j+1]) in
		  get_ranges_and_ext_pred_of_arg (j+1) tl ranges (a_pred::ext_pred) 
                    string_predicates sign
	      with Not_found ->
		let (id,sign) = 
                  if a=""
                  then (Datalog_signature.add_eq_get_id sign)
                  else (Datalog_signature.add_pred_get_id 2 a sign)
                in
		let string_predicates = String_map.add a id string_predicates in
		let a_pred = Program.Pred (id,[j;j+1]) in
		  get_ranges_and_ext_pred_of_arg (j+1) tl ranges (a_pred::ext_pred) 
                    string_predicates sign
	    )

      )
    in
    let pred_of_rhs ranges var_predicate rhs_cat= 
      (match rhs_cat with
	  Rhs_cat(id,vars) ->
            let (arg,var_predicate,_) =
              List.fold_left
		(function (l,var_predicate,n) ->
		  function v ->
		    let ((i,j),_) = 
                      try Int_map.find v ranges 
                      with Not_found -> failwith "pred_of_rhs"
                    in
		      (l@[i;j],Int_map.add v (id,n) var_predicate,n+1)
		)
		([],var_predicate,0)
		vars
            in
	      (Program.Pred(id,arg),var_predicate)
      )
    in
    let eq_pred ranges eq_predicates var_predicate sign= 
      Int_map.fold
        (function v ->
          function ((i,j),eq_list) ->
            function (eq_pred_list,sign,eq_predicates) ->
              if eq_list = []
              then (eq_pred_list,sign,eq_predicates)
              else
                let (pred,kth) = 
                  try 
                    Int_map.find v var_predicate 
                  with Not_found -> failwith "eq_pred"
                in
                let (id,sign,eq_predicates) = 
                  (try
                      (Pair_map.find (pred,kth) eq_predicates, sign, eq_predicates)
                    with Not_found ->
                      let name = (gen_eq_name pred kth sign) in
                      let (id,sign) = Datalog_signature.add_pred_get_id 4 name sign in
                        (id,sign,Pair_map.add (pred,kth) id eq_predicates)
                  )
                in
                  (eq_pred_list @
                      (List.rev_map 
                          (function (k,l) ->
                            Program.Pred(id,[i;j;k;l])
                          )
                          eq_list
                      ),
                  sign,
                  eq_predicates)
        )
        ranges
        ([],sign,eq_predicates)
    in
    let get_var_pred rhs=
      List.fold_left
        (function var_predicate ->
          function Rhs_cat(pred,l)->
            let (var_predicate,_) = 
              List.fold_left
                (function (var_predicate,n) ->
                  function v ->
                    (Int_map.add v (pred, n) var_predicate,n+1)
                )
                (var_predicate,0)
                l
            in
              var_predicate
        )
        Int_map.empty
        rhs
    in
    let generate_eq_clauses eq_predicates string_predicates sign grammar=
      let gen_eq_clause id kth (R(Lhs_cat(_,args),rhs)) sign eq_predicates eq_pred_to_create= 
        let kth_arg = List.nth args kth in
        let var_predicate = get_var_pred rhs in
        let k = (List.length kth_arg)+1 in
        let (j,l,_,eq_rhs,sign,eq_predicates,eq_pred_to_create) = 
          List.fold_left
            (function (j,l,ranges,eq_rhs,sign,eq_predicates,eq_pred_to_create)->
              function arg ->
                match arg with
                    Var v ->
                      (try
                          let (s,e) = Int_map.find v ranges in
                          let id = Pair_map.find (Int_map.find v var_predicate) eq_predicates in
                          let eq_rhs = eq_rhs@[(Program.Pred(id,[s;e;j;j+1]));(Program.Pred(id,[s;e;l;l+1]))] in
                            (j+1,l+1,ranges,eq_rhs,sign,eq_predicates,eq_pred_to_create)
                        with Not_found ->
                          let (pred,arg_nb) = Int_map.find v var_predicate in
                          let (id,sign,eq_predicates,eq_pred_to_create) = 
                            try
                              (Pair_map.find (pred,arg_nb) eq_predicates,sign,eq_predicates,eq_pred_to_create)
                            with Not_found ->
                              let name = (gen_eq_name pred kth sign) in
                              let (id,sign) = Datalog_signature.add_pred_get_id 4 name sign in
                                (id,sign,Pair_map.add (pred,kth) id eq_predicates,((pred,kth),id)::eq_pred_to_create)
                          in
                          let eq_rhs = eq_rhs@[Program.Pred(id,[j;j+1;l;l+1])] in
                            (j+1,l+1,Int_map.add v (j,j+1) ranges,eq_rhs,sign,eq_predicates,eq_pred_to_create)
                      )
                  | Val s ->
                      let id = String_map.find s string_predicates in
                      let eq_rhs = eq_rhs@[Program.Pred(id,[j;j+1]); Program.Pred(id,[l;l+1])] in
                        (j+1,l+1,ranges,eq_rhs,sign,eq_predicates,eq_pred_to_create)
            )
            (0, k, Int_map.empty, [],sign,eq_predicates,eq_pred_to_create)
            kth_arg
        in
          (Program.Cl(Program.Pred(id,[0;j;k;l]),eq_rhs),sign,eq_predicates,eq_pred_to_create)
      in
      let rec gen_all_eq eq_clauses sign eq_predicates eq_pred_to_create= 
        (match eq_pred_to_create with
            [] -> (eq_clauses,sign)
          | ((pred,kth),id) :: eq_pred_to_create ->
              let (eq_clauses,sign,eq_predicates,eq_pred_to_create) = 
                List.fold_left
                  (function (eq_clauses,sign,eq_predicates,eq_pred_to_create)->
                    function grule ->
                      let (clause,sign,eq_predicates,eq_pred_to_create) =
                        gen_eq_clause id kth grule sign eq_predicates eq_pred_to_create
                      in
                        (eq_clauses@[clause],sign,eq_predicates,eq_pred_to_create)
                  )
                  (eq_clauses,sign,eq_predicates,eq_pred_to_create)
                  (get_rules_defining_cat pred grammar)
              in
                gen_all_eq eq_clauses sign eq_predicates eq_pred_to_create
        )
      in
        gen_all_eq [] sign eq_predicates
          (Pair_map.fold 
          (function p ->
            function id ->
             function eq_pred_to_create -> (p,id)::eq_pred_to_create
          )
          eq_predicates
          []
          )
          
    in
    let clause_of_rule grule string_predicates sign eq_predicates = 
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
            let lhs =Program.Pred(id,vars) in
            let rhs_cl = List.rev ext_pred in
            let (rhs_cl,var_predicate) = 
              List.fold_left
                (function (rhs_cl,var_predicate) ->
                  function rhs_cat ->
                    let (rhs_pred,var_predicate) = pred_of_rhs ranges var_predicate rhs_cat in
                      (rhs_cl@[rhs_pred], var_predicate)
                )
                (rhs_cl,Int_map.empty)
                rhs
            in  
            let (eq_pred_list, sign, eq_predicates) = eq_pred ranges eq_predicates var_predicate sign in
              (Program.Cl(lhs,rhs_cl@eq_pred_list),string_predicates,sign,eq_predicates)
      )
    in
    let (clauses,string_predicates,new_sign,eq_predicates) = 
      List.fold_left
        (function (clauses,string_predicates,sign,eq_predicates) ->
          function grule ->
            let (clause,string_predicates,sign,eq_predicates) = 
              clause_of_rule grule string_predicates sign eq_predicates
            in
              (clause::clauses,string_predicates,sign,eq_predicates)
	)
	([],string_predicates,new_sign,Pair_map.empty)
	rules
    in
    let (eq_clauses,new_sign) = generate_eq_clauses eq_predicates string_predicates new_sign grammar in
      Program.Prog (new_sign,(clauses @ eq_clauses))
        
end
