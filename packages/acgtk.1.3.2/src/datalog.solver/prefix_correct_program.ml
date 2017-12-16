open Datalog_signature
open Int_map
open Program
open Int_set

module Prefix_correct_program =
struct
  
  type prog_context = 
      Prog_context of 
        Datalog_signature.signature *
          ((int list) Int_map.t) *
          (Program.clause list)
    

  type clause_context_fst_pass = 
      Cl_fst_pass of
        prog_context *
          Program.predicate *
          Program.predicate list

  type snd_pass_prog_context = 
      Prog_context_snd_pass of 
        Datalog_signature.signature *
          ((int list) Int_map.t) *
          (Program.clause list) *
          int

  type clause_context_snd_pass = 
      Cl_snd_pass of
        Datalog_signature.signature* (*the signature we currently work in*)
          ((int list) Int_map.t)* (*added_predicates*)
          (Program.clause list) * (*clauses collected so far*)
          int * (*nb of treated clauses*)
          Int_set.t *   (*the set of variables that are used in the treated predicates*)
          Program.predicate * (*the rhs of the next clause*)
          (Program.predicate list) * (*predicates from lhs that remain to be treated*)
          (int list) *    (*the vars that remain to be treated*)
          (int list)    (*the new predicates that remain to be treated*)
          
          
          
  let init_prog_context exceptions sign=
    let rec add_pred 
        rank name n sign l=
      if n=rank
      then (sign,l)
      else 
        let id = Datalog_signature.fresh sign in
        let sign = 
          Datalog_signature.add_pred n (name^"_"^(string_of_int (n/2))) sign 
        in
          add_pred rank name (n+2) sign (id::l)
    in
      (match sign with 
          Datalog_signature.S (decl,_,_)->
            let (_,new_sign,added_predicates)=
              List.fold_left
                (function (n,sign,added_predicates) ->
                  function (rank,name) ->
                    if exceptions name
                    then 
                      let added_predicates =
                        Int_map.add n [] added_predicates 
                      in
                        (n+1,sign,added_predicates)
                    else
                      let (new_sign,l) = 
                        add_pred rank name 2 sign []
                      in
                      let added_predicates =
                        Int_map.add n l added_predicates
                      in
                        (n+1,new_sign,added_predicates)
                )
                (0,sign,Int_map.empty)
                decl 
            in
              Prog_context (new_sign,added_predicates,[])
      )

  let extract_result prog_context=
    let Prog_context(sign,_,clauses)=prog_context in
      Program.Prog(sign,clauses)

  let init_clause_fst_pass_context prog_context rhs = 
    Cl_fst_pass(prog_context,rhs,[])

  let get_the_two_first_vars vars = 
    match vars with
        v1::v2::vars -> (v1,v2,vars)
      |_ -> failwith "get_the_two_first_vars: contains less that two elements"

  let drop_the_two_last_vars vars =
    let (_,_,vars) = get_the_two_first_vars (List.rev vars) in
      List.rev vars

  let append_clauses_to_prog_context prog_context clauses = 
    let Prog_context(sign,added_predicates,new_clauses) = 
      prog_context 
    in
      Prog_context(sign,added_predicates,new_clauses@clauses)

  let append_predicates_to_fst_pass_clause_context clause_context preds= 
    let Cl_fst_pass(prog_context,rhs,new_lhs) = clause_context in
      Cl_fst_pass(prog_context,rhs,new_lhs@preds) 

  let get_added_predicates_from_prog_context prog_context k =
    let Prog_context(_,added_predicates,_) = prog_context in
      Int_map.find k added_predicates

 let get_added_predicates_from_snd_pass_prog_context prog_context k =
    let Prog_context_snd_pass(_,added_predicates,_,_) = prog_context in
      Int_map.find k added_predicates
        
  let get_added_predicates_from_clause_fst_pass clause_context k =
    let Cl_fst_pass(prog_context,_,_) = clause_context in
      get_added_predicates_from_prog_context prog_context k

        
  let update_clause_fst_pass clause_context pred = 
      (match pred with 
          Program.Pred(k,vars) ->
            let l = 
              get_added_predicates_from_clause_fst_pass 
                clause_context 
                k 
            in
            let (_,preds)= 
              List.fold_left
                (function (vars,preds) ->
                  function k ->
                    let vars = 
                      drop_the_two_last_vars vars
                    in
                      (vars, (Program.Pred(k,vars))::preds)
                )
                (vars,[pred])
                l
            in
              append_predicates_to_fst_pass_clause_context 
                clause_context
                preds
      )

  let extract_result_fst_pass_clause_context clause_context =
    let Cl_fst_pass(prog_context,rhs,new_lhs) =clause_context in
    let clause = Program.Cl(rhs,new_lhs) in
     append_clauses_to_prog_context prog_context [clause]


  let first_pass_transform exceptions= 
    Program.transform_left
      (init_prog_context  exceptions)
      (function prog_context -> prog_context)
      init_clause_fst_pass_context
      update_clause_fst_pass
      extract_result_fst_pass_clause_context

  let init_snd_pass_prog_context prog_context = 
    let Prog_context (signature,added_predicates,_) = prog_context in
      Prog_context_snd_pass(signature,added_predicates,[],0)

  let extract_prog_from_snd_pass_prog_context prog_context= 
    let Prog_context_snd_pass(signature,_,clauses,_)=prog_context in
      Program.Prog(signature,clauses)

  let init_snd_pass_clause_context prog_context clause =
    let Program.Cl(rhs,lhs) = clause in
    let Program.Pred(k,vars) =rhs in
    let vars_set = 
      List.fold_left
        (function set ->
          function v ->
            Int_set.add v set
        )
        Int_set.empty
        vars
    in
    let Prog_context_snd_pass(sign,added_predicates,new_clauses,n) = prog_context in
    let l = get_added_predicates_from_snd_pass_prog_context prog_context k in
      Cl_snd_pass (
        sign, 
        added_predicates,
        new_clauses,
        n,
        vars_set,
        rhs,
        lhs,
        List.rev vars,
        l
      )

  
  let is_snd_pass_clause_context_exhausted clause_context = 
    let Cl_snd_pass(_,_,_,_,_,_,_,_,remaining_preds) =clause_context in
      remaining_preds=[]

  let extract_prog_context_from_snd_pass_clause clause_context = 
    let Cl_snd_pass
        (signature,
        added_predicates,
        new_clauses,
        n,
        used_vars,
        new_rhs,
        remaining_lhs,
        remaining_vars,
        remaining_preds
        )= clause_context 
    in
    let clause = Program.Cl(new_rhs,remaining_lhs) in
    let new_clauses = new_clauses@[clause]
    in
      Prog_context_snd_pass(signature,added_predicates,new_clauses,n+1)

  let find_remove prop list = 
    let rec find_remove_rec pref suff =
      match suff with
          [] -> failwith "find_remove: not found"
        | a::suff -> 
            (if prop a
            then (a,List.rev_append pref suff)
              else find_remove_rec (a::pref) suff
            )
    in
      find_remove_rec [] list

  let rec get_next a list =
    match list with
       b::c::list ->
          if a=b 
          then c
          else get_next a list
      |_  -> failwith "get_next: not found"

  let rec separate_predicates_between_i_and_j i j predicates collected_predicates = 
    if i=j
    then (predicates,List.rev collected_predicates)
    else
      let (pred_j,predicates) = 
        find_remove
          (function Program.Pred(_,vars) -> List.mem j vars)
          predicates
          (*the construction assures that the index j is contained in exactly one predicate*)
      in
      let Program.Pred(_,vars) = pred_j in
      let new_j = List.hd(List.tl(List.rev vars)) in
        separate_predicates_between_i_and_j i new_j predicates (pred_j::collected_predicates)

  let get_vars_from_predicates prop pred_list = 
    let rec get_vars_rec var_set pred_list = 
      match pred_list with
          [] -> Int_set.elements var_set
        | pred::pred_list ->
            let Program.Pred(_,vars) = pred
            in 
            let var_set = 
              List.fold_left
                (function set ->
                  function v -> 
                    if(prop v)
                    then Int_set.add v set
                    else set
                )
                var_set
                vars
            in get_vars_rec var_set pred_list
    in
      get_vars_rec Int_set.empty pred_list 

  let rec add_vars_from_predicates var_set pred_list = 
    match pred_list with
        [] -> var_set
      | (Program.Pred(_,vars))::pred_list ->
          let var_set =
            List.fold_left
              (function set ->
                function v -> Int_set.add v set)
              var_set
              vars
          in
            add_vars_from_predicates var_set pred_list
            
  let update_snd_pass_clause_context clause_context = 
    let Cl_snd_pass
        (signature,
        added_predicates,
        new_clauses,
        n,
        used_vars,
        new_rhs,
        remaining_lhs,
        remaining_vars,
        remaining_preds
        )
        =
      clause_context
    in
    let (v2,v1,new_remaining_vars) = get_the_two_first_vars remaining_vars in
    let (remaining_lhs,from_v1_to_v2) = 
      separate_predicates_between_i_and_j v1 v2 remaining_lhs [] 
    in
    let used_vars = add_vars_from_predicates used_vars from_v1_to_v2 in
    let vars =
      get_vars_from_predicates 
        (function v -> Int_set.mem v used_vars) 
        remaining_lhs
    in
    let (id,signature) = 
      Datalog_signature.add_pred_get_id 
        (List.length vars) 
        ("aux_"^(string_of_int n)^"_"^(string_of_int (List.length remaining_preds))) 
        signature 
    in
    let aux_pred = Program.Pred(id,vars) in
    let (p,remaining_preds) = (List.hd remaining_preds, List.tl remaining_preds) in
    let new_pred = Program.Pred(p,List.rev new_remaining_vars) in
    let clause1 = Program.Cl(new_rhs,aux_pred::from_v1_to_v2) in
    let clause2 = Program.Cl(new_pred,[aux_pred]) in
    let new_clauses = new_clauses@[clause1;clause2] in
      Cl_snd_pass
        (signature,
        added_predicates,
        new_clauses,
        n,
        used_vars,
        aux_pred,
        remaining_lhs,
        new_remaining_vars,
        remaining_preds
        )

  let rec fix_point_snd_pass_clause_context clause_context=
    if is_snd_pass_clause_context_exhausted clause_context
    then extract_prog_context_from_snd_pass_clause clause_context
    else fix_point_snd_pass_clause_context
      (update_snd_pass_clause_context clause_context)

  let update_snd_pass_prog_context prog_context clause= 
    let clause_context = init_snd_pass_clause_context prog_context clause in
      fix_point_snd_pass_clause_context clause_context

  let perform_snd_pass prog_context = 
    let Prog_context(_,_,clauses) = prog_context in
    let prog_context = init_snd_pass_prog_context prog_context in
    let prog_context = 
      List.fold_left
        update_snd_pass_prog_context
        prog_context
        clauses
    in
      extract_prog_from_snd_pass_prog_context prog_context

  (*let program_after_first_pass = 
    Program.transform_left
      init_prog_context
      extract_result
      init_clause_fst_pass_context
      update_clause_fst_pass
      extract_result_fst_pass_clause_context*)

  let prefix_correct_transform exceptions program = 
    let prog_context = first_pass_transform exceptions program in
      perform_snd_pass prog_context

        
end
