%{

    let id_to_term (id,loc) typing_env sg warnings =
      match Environment.Signature1.is_constant id sg,Typing_env.mem id typing_env with
      | (true,_),false -> Abstract_syntax.Const (id,loc),loc,warnings
      | (false,_),true -> Abstract_syntax.Var (id,loc),loc,warnings
      | (true,_),true -> Abstract_syntax.Var (id,loc),loc,(Error.Variable_or_constant (id,loc))::warnings
      | (false,_),false -> emit_parse_error (Error.Unknown_constant_nor_variable id) loc
    %}


%token <Logic.Abstract_syntax.Abstract_syntax.location> LAMBDA
%token <Logic.Abstract_syntax.Abstract_syntax.location> LAMBDA0
%token <Logic.Abstract_syntax.Abstract_syntax.location> DOT
                                    
                                                                                                                                          %start < AcgData.Environment.Environment.Signature1.t -> Logic.Lambda.Lambda.term * Logic.Lambda.Lambda.stype > term_alone
%start < AcgData.Environment.Environment.Signature1.t -> AcgData.Environment.Environment.Signature1.t -> Logic.Abstract_syntax.Abstract_syntax.term * Logic.Abstract_syntax.Abstract_syntax.type_def > heterogenous_term_and_type

                                                                                                                                          %%

%public term_alone:
| t = term COLON ty = type_expression EOI
  {
    fun sg ->
    let t,_,ws = t Typing_env.empty sg [] in
    let ty,_ = ty sg in
    Environment.Signature1.convert_term t ty sg
  }

%public heterogenous_term_and_type:
| t = term COLON ty = type_expression EOI
  {
    fun abs obj ->
    let t,_,ws = t Typing_env.empty obj [] in
    let ty,_ = ty abs in
    t,ty
  }

  
%public term:
| t = atomic_type_or_term { 
          fun type_env sg warnings ->
          id_to_term t type_env sg warnings
        }
| t = not_atomic_term { t }

%public atomic_type_or_term:
| id = IDENT { id }
             (*| LPAREN t = atomic_type_or_term RPAREN { t } *)

             ident_sequence:
| id0 = IDENT ids = IDENT+ { id0,ids }
                                 
%public not_atomic_term:
| id_seq = ident_sequence
             {
               fun type_env sg warnings ->
               let id1,ids = id_seq in
               let t,t_loc,ws = id_to_term id1 type_env sg warnings in
               let res,res_loc,ws' =
                 List.fold_left
                   (fun (acc,loc,ws) id ->
                     let t',l',ws' = id_to_term id type_env sg ws in
                     let loc' = new_loc loc l' in
                     Abstract_syntax.App (acc,t',loc'),loc',ws')
                   (t,t_loc,ws)
                   ids in
               res,res_loc,ws'
             }
| id_seq = IDENT* sym = SYMBOL terms = term0*
  {
    fun type_env sg warnings ->
    let sym_name,sym_loc = sym in
    let sym_term =
      match Environment.Signature1.is_constant sym_name sg with
      | true,Some fix -> Term_sequence.Op (Abstract_syntax.Const (sym_name,sym_loc),fix,sym_loc)
      | true,None -> failwith "Bug: Should no happen"
      | false,_ -> emit_parse_error (Error.Unknown_constant sym_name) sym_loc in
    let id_seq',ws =
      List.fold_left
        (fun (acc,ws) id ->
          let t,l,ws' = (id_to_term id type_env sg ws) in
          (Term_sequence.Term (t,l))::acc,ws')
        ([],warnings)
        id_seq in
    let terms,warnings' =
      List.fold_left
        (fun (lst,ws) t ->
          let t',ws'= t type_env sg ws in
          t'::lst,ws')
        (sym_term::id_seq',ws)
        terms  in
    let result,result_loc = Term_sequence.parse_sequence (List.rev terms) sg in
    result,result_loc, warnings'
  }
| id_seq = IDENT+ LPAREN t = term RPAREN terms = term0*
  {
    fun type_env sg warnings ->
    let term,loc,ws = t type_env sg warnings in
    let term' = Term_sequence.Term (term,loc) in
    let id_seq',ws' =
      List.fold_left
        (fun (acc,ws) id -> 
          let t,l,ws' = (id_to_term id type_env sg ws) in
          (Term_sequence.Term (t,l))::acc,ws')
        ([],ws)
        id_seq in
    let terms,warnings =
      List.fold_left
        (fun (lst,ws) t ->
          let t',ws'= t type_env sg ws in
          t'::lst,ws')
        (term'::id_seq',ws')
        terms  in
    let result,result_loc = Term_sequence.parse_sequence (List.rev terms) sg in
    result,result_loc, warnings
  }
|  LPAREN t = not_atomic_term RPAREN terms = term0*
  {
    fun type_env sg warnings ->
    let term,loc,ws = t type_env sg warnings in
    let term' = Term_sequence.Term (term,loc) in
    let terms,warnings =
      List.fold_left
        (fun (lst,ws) t ->
          let t',ws'= t type_env sg ws in
          t'::lst,ws')
        ([],ws)
        terms  in
    let result,result_loc = Term_sequence.parse_sequence (term'::(List.rev terms)) sg in
    result,result_loc, warnings
  }

| t = bound_term { t }


%inline bound_term :
| binder = LAMBDA vars_and_term = bound_term_ending 
                                 {
                                   fun type_env sg warnings ->
                                   let vars,t = vars_and_term in
                                   let type_env = List.fold_left
                                                    (fun acc (var,_) -> Typing_env.add var acc)
                                                    type_env
                                                    vars in                                   
                                   let t',loc',ws' = t type_env sg warnings in
                                   let n_loc = new_loc binder loc' in
                                   ((fst (List.fold_left
                                       (fun (acc,first_var) (var,loc) ->
                                         if first_var then
                                           (fun t -> acc (abs (var,n_loc,t) Abstract_syntax.Non_linear)),false
                                         else
                                           (fun t -> acc (abs (var,loc,t) Abstract_syntax.Non_linear)),false)
                                       ((fun x -> x),true)
                                       vars))
                                      t'),
                                   n_loc,
                                   ws'}
| binder = LAMBDA0 vars_and_term = bound_term_ending
                                     {
                                       fun type_env sg warnings ->
                                       let vars,t = vars_and_term in
                                       let type_env = List.fold_left
                                                        (fun acc (var,_) -> Typing_env.add var acc)
                                                        type_env
                                                        vars in                                   
                                       let t',loc',ws' = t type_env sg warnings in
                                       let n_loc = new_loc binder loc' in
                                       ((fst (List.fold_left
                                           (fun (acc,first_var) (var,loc) ->
                                             if first_var then
                                               (fun t -> acc (abs (var,n_loc,t) Abstract_syntax.Linear)),false
                                             else
                                               (fun t -> acc (abs (var,loc,t) Abstract_syntax.Linear)),false )
                                           ((fun x -> x),true)
                                           vars))
                                          t'),
                                       n_loc,
                                       ws' }
| binder =  IDENT vars_and_term = bound_term_ending
                                    {
                                      fun type_env sg warnings ->
                                      let binder,loc = binder  in
                                      let vars,t = vars_and_term  in
                                      let linearity =
                                        match Environment.Signature1.is_constant binder sg with
                                        | true,Some Abstract_syntax.Binder ->
                                           (match Environment.Signature1.get_binder_argument_functional_type binder sg with
					    | None -> failwith "Bug: Binder of non functional type"
					    | Some k -> k)
				        | _ -> emit_parse_error (Error.Binder_expected binder) loc in
                                      let type_env = List.fold_left
                                                       (fun acc (var,_) -> Typing_env.add var acc)
                                                       type_env
                                                       vars in                                   
                                      let t',loc',ws' = t type_env sg warnings in
                                      let n_loc = new_loc loc loc' in
                                      Abstract_syntax.App
                                        (Const (binder,loc),
                                         (fst (List.fold_left
                                            (fun (acc,first_var) (var,loc) ->
                                              if first_var then
                                                (fun t -> acc (abs (var,n_loc,t) linearity )),false
                                              else
                                                (fun t -> acc (abs (var,loc,t) linearity )),false)
                                            ((fun x -> x),true)
                                            vars))
                                           t',
                                         n_loc),
                                      n_loc,
                                      ws' }
                                    
                                    bound_term_ending :
| var = IDENT DOT t=term {[var],t}
| var = IDENT vars_and_term = bound_term_ending {let vars,t = vars_and_term in var::vars,t}
                                 
  term0:
| id = atomic_type_or_term
         {
           fun type_env sg warnings ->
           let id,loc = id in
           let t,ws =
             match Environment.Signature1.is_constant id sg,Typing_env.mem id type_env with
             | (true,_),false -> Abstract_syntax.Const (id,loc),warnings
             | (false,_),true -> Abstract_syntax.Var (id,loc),warnings
             | (true,_),true -> Abstract_syntax.Var (id,loc),(Error.Variable_or_constant (id,loc))::warnings
	     | (false,_),false -> emit_parse_error (Error.Unknown_constant_nor_variable id) loc in
           Term_sequence.Term (t,loc),ws }
         
| id = SYMBOL
         {
           fun type_env sg warnings ->
           let name,loc = id in
           match Environment.Signature1.is_constant name sg with
           | true,Some fix -> Op (Abstract_syntax.Const (name,loc),fix,loc),warnings
           | true,None -> failwith "Bug: Should no happen"
           | false,_ -> emit_parse_error (Error.Unknown_constant name) loc
         }

| LPAREN id = SYMBOL RPAREN
                     {
                       fun type_env sg warnings ->
                       let name,loc = id in
                       let t = Abstract_syntax.Const (name,loc) in
                       Term_sequence.Term (t,loc),warnings
                     }
                     
| LPAREN t = not_atomic_term RPAREN
                             {
                               fun type_env sg warnings ->
                               let term,loc,ws = t type_env sg warnings in
                               Term_sequence.Term (term,loc),ws }


