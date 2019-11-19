open Adornment2
open Datalog_signature
open Program
open Int_map


module Magic_set_rewritting = 
struct
  
  module Adorned_predicate_map=Map.Make (Adornment.Ordered_adorned_predicates)

  type prog_context = 
      Prog_context of
        Datalog_signature.signature*
          ((int*int) Adorned_predicate_map.t) *
          int *
          (Program.clause list)* (*group_I*)
          (Program.clause list)* (*group_III*)
          (Program.clause list) (*group_IV*)

  type clause_context = 
      Cl_context of
        Datalog_signature.signature * (*signature that is enriched with magic and supplementary predicates*)
          ((int*int)  Adorned_predicate_map.t) * (*maps adorned predicates to their normal and magic counter-parts; when the two numbers are equal, this means that the predicate is extensional*)
          string * (*prefix of the names of the new supplementary predicates*)
          int * (* number 'i' of the  current supplementary predicate *)
          int * (* rule number *)
          (int list) * (* list of the variables of the rhs*)
          (int list) * (* list of the bound variables *)
          (int list) * (* list of the variables that appear in the ith first predicates *)
          Program.predicate * (* current supplementary predicate *)
          Program.predicate * (* initial right hand side *)
          (Adornment.adornment list) * (* remaining of the left hand side *)
           (Program.clause list)* (*group_I*)
          (Program.clause list)* (*group_III*)
          (Program.clause list) (*group_IV*)
      
          
  let sup_name k = "sup_"^(string_of_int k)^"_"

  let mark_intensionality ad_program = 
    let Adornment.AProg(Datalog_signature.S (l,_,_),_,clauses) = ad_program in
    let n = (List.length l) - 1 in
    let rec intensional res n = 
      if n=(-1)
      then res 
      else 
        let b = 
          List.exists 
            (function Adornment.ACl(Adornment.Ad(k,_),_) -> k=n)
            clauses
        in
          intensional (b::res) (n-1)
    in
      intensional [] n

  let is_intensional l k = List.nth l k

  let count_bound_vars ad_pred = 
    let Adornment.AdP(_,ad) = ad_pred  in
      List.fold_left 
        (function n ->
          function b -> 
            if b then n+1 else n
        )
        0 
        ad      

  let add_var var_list var = 
    if List.mem var var_list
    then var_list
    else var_list@[var]

  let get_bound_vars ad = 
    match ad with
        Adornment.Ad(_,ad_vars)->
          List.fold_left
            (function var_list ->
              function (var,b) ->
                if b
                then add_var var_list var
                else var_list
            )
            []
            ad_vars
            
  let get_vars ad = 
    match ad with
        Adornment.Ad(_,ad_vars)->
          List.fold_left
            (function var_list ->
              function (var,_) ->
                add_var var_list var
            )
            []
            ad_vars

  let add_vars var_list ad = 
    let Adornment.Ad(_,ad_vars) = ad in
      List.fold_left
        (function var_list ->
          function(var,_) -> add_var var_list var
        )
        var_list
        ad_vars

  let get_vars_from_list ad_list = 
    List.fold_left
      add_vars
      []
      ad_list

  let get_ad_bound_vars ad = 
    match ad with
        Adornment.Ad(_,ad_vars)->
          List.fold_left
            (function var_list ->
              function (var,b) ->
                if b
                then var_list@[var]
                else var_list
            )
            []
            ad_vars

  let get_ad_vars ad = 
    match ad with
        Adornment.Ad(_,ad_vars)->
          fst(List.split ad_vars)



  let get_new_signature ad_program = 
    let is_intensional = 
      (function Adornment.AdP(k,_) ->
        is_intensional (mark_intensionality ad_program) k
      )
    in
    let Adornment.AProg(sign,interesting_adornments,ad_clauses) = ad_program in
    let Datalog_signature.S(_,eq,neq) = sign in
    let ad_list = Adornment.Adorned_predicate_set.elements interesting_adornments in
    let (_,l,map,_,new_eq,new_neq) =
      List.fold_left
        (function (n,l,map, extensional_map,eq_opt,neq_opt) ->
          function ad_pred ->
            (if is_intensional ad_pred
              then 
              let (arity,name) = Adornment.predicate_of_adornment sign ad_pred in
              let p = count_bound_vars ad_pred in
                (n+2, 
                (p,"m_"^name)::(arity,name)::l, 
                Adorned_predicate_map.add ad_pred (n,n+1) map, 
                extensional_map,eq_opt,neq_opt)
              else
                let Adornment.AdP(pred,_) = ad_pred in
                  try 
                    let k = Int_map.find pred extensional_map in
                      (n,
                      l,
                      Adorned_predicate_map.add ad_pred (k,k) map, 
                      extensional_map,eq_opt,neq_opt)
                  with 
                      Not_found ->
                        let (arity,name) = Datalog_signature.get_predicate pred sign in
                        let new_eq = if(Some(pred)=eq) then Some(n) else eq_opt in
                        let new_neq = if(Some(pred)=neq) then Some(n) else neq_opt in
                          (n+1,
                          (arity,name)::l, 
                          Adorned_predicate_map.add ad_pred (n,n) map, 
                          Int_map.add pred n extensional_map,new_eq,new_neq)
            )
        )
        (0,[],Adorned_predicate_map.empty,Int_map.empty,None,None)
        ad_list
    in
      (Datalog_signature.S(List.rev l,new_eq,new_neq), map)

  let init_prog_context ad_program = 
    let (sign,map) = get_new_signature ad_program in
      Prog_context (sign,map,1,[],[],[])

  let init_clause_context prog_context clause= 
    let Prog_context(sign,map,rule_nb,group_I,group_III,group_IV) = prog_context in
    let Adornment.ACl(ad,lhs) = clause in
    let sup_name = sup_name rule_nb in
    let (p,magic_p) = Adorned_predicate_map.find (Adornment.to_adorned_predicate ad) map in
    let rhs_vars = get_vars ad in
    let bound_vars = get_bound_vars ad in
    let initial_rhs = Program.Pred(p,get_ad_vars ad) in
    let magic_pred = Program.Pred(magic_p,get_ad_bound_vars ad) in
      Cl_context(
        sign,
        map,
        sup_name,
        1,
        rule_nb,
        rhs_vars,
        bound_vars,
        [],
        magic_pred,
        initial_rhs,
        lhs,
        group_I,
        group_III,
        group_IV)

  let get_pred_and_magic_pred clause_context ad=
    let Cl_context (_,map,_,_,_,_,_,_,_,_,_,_,_,_) = clause_context in
      Adorned_predicate_map.find (Adornment.to_adorned_predicate ad) map

  let update_clause_context clause_context= 
    let Cl_context(
      sign,
      map,
      sup_name,
      i,
      rule_nb,
      rhs_vars,
      bound_vars,
      vars_appearing_in_the_ith_first_pred,
      sup_pred,
      initial_rhs,
      remaining_lhs,
      group_I,
      group_III,
      group_IV)
        =
      clause_context
    in
    let (ad,remaining_lhs) = (List.hd remaining_lhs,List.tl remaining_lhs) in
    let vars_appearing_in_the_ith_first_pred = add_vars vars_appearing_in_the_ith_first_pred ad in
    let vars_appearing_after_the_ith_first_pred = get_vars_from_list remaining_lhs in
    let sup_vars =
      List.fold_left
        add_var
        bound_vars
        (List.filter
            (function v ->
              (List.mem v rhs_vars)
              ||
                (List.mem v vars_appearing_after_the_ith_first_pred)
            )
            vars_appearing_in_the_ith_first_pred
        )
    in
    let arity = List.length sup_vars in
    let name = sup_name^(string_of_int i) in
    let (new_sup_pred,sign) = 
      if remaining_lhs=[]
      then (initial_rhs,sign)
      else 
        let (sup_p,sign) = Datalog_signature.add_pred_get_id arity name sign in
        let new_sup_pred = Program.Pred(sup_p,sup_vars) in
          (new_sup_pred,sign)
    in
    let (p, magic_p) = get_pred_and_magic_pred clause_context ad in
    let vars = get_ad_vars ad in
    let pred = Program.Pred(p,vars) in
    let clause = Program.Cl(new_sup_pred,[sup_pred;pred]) in
    let (group_III,group_IV) = 
      if remaining_lhs=[]
      then (group_III,group_IV@[clause])
      else
        (group_III@[clause],group_IV)
    in
    let group_I = 
      if(p=magic_p)
      then group_I
      else 
        let bound_vars_of_p = get_ad_bound_vars ad in
        let magic_pred = Program.Pred(magic_p,bound_vars_of_p) in
        let magic_clause = Program.Cl(magic_pred,[sup_pred]) in
          group_I@[magic_clause]
    in
      Cl_context(
        sign,
        map,
        sup_name,
        i+1,
        rule_nb,
        rhs_vars,
        bound_vars,
        vars_appearing_in_the_ith_first_pred,
        new_sup_pred,
        initial_rhs,
        remaining_lhs,
        group_I,
        group_III,
        group_IV
      )

  let is_finished_clause_context clause_context= 
    match clause_context with
        Cl_context(_,_,_,_,_,_,_,_,_,_,[],_,_,_) -> true
      |_ -> false

  let extract_clause_context clause_context = 
    let Cl_context(sign,map,_,_,rule_nb,_,_,_,_,_,_,group_I,group_III,group_IV) 
        = clause_context
    in
      Prog_context(sign,map,rule_nb+1,group_I,group_III,group_IV)

  let rec process_clause_context clause_context = 
    if is_finished_clause_context clause_context
    then extract_clause_context clause_context
    else process_clause_context (update_clause_context clause_context)

  let process_clause prog_context clause = 
    let clause_context = init_clause_context prog_context clause in
      process_clause_context clause_context


  let extract_prog_context prog_context = 
    let Prog_context(sign,map,_,group_I,group_III,group_IV)=prog_context in
      (map, Program.Prog(sign,group_I@group_III@group_IV))

  let magic ad_program = 
    let prog_context = init_prog_context ad_program in
    let Adornment.AProg(_,_,ad_clauses) = ad_program in
    let prog_context = 
      List.fold_left
        process_clause
        prog_context
        ad_clauses
    in
      extract_prog_context prog_context
end
