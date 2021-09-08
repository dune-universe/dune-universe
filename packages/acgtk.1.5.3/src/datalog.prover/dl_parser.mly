%{
  open UtilsLib
  open IdGenerator
  open Datalog_AbstractSyntax

  let check_arity (pred_sym,loc) length arity =
    let () = Logs.info (fun m -> m "Checking symbol \"%s\". Parameter number is %d" pred_sym length) in
    match arity with
    | Some a when a<>length ->
       let () = Logs.info (fun m -> m "Found arity %d" a) in
       raise DlError.(Error.(Error (SyntError (BadArity (pred_sym,a,length)),loc)))
    | _ -> ()


         
%}

%token <string*UtilsLib.ErrorMg.location> IDENT
%token <int*UtilsLib.ErrorMg.location> INT
%token <UtilsLib.ErrorMg.location>LPAR RPAR COMMA DOT FROM SLASH QUESTION_MARK
%token EOI


%start rule program extensional_facts query
%type < Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > rule
%type < Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > program
%type < (Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table) -> (Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table) > query
%type < (Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table*UtilsLib.IdGenerator.IntIdGen.t) -> (Datalog_AbstractSyntax.AbstractSyntax.Rule.rule list*Datalog_AbstractSyntax.ConstGen.Table.table*UtilsLib.IdGenerator.IntIdGen.t) > extensional_facts
   
%%
  
  let program :=
    | rules=nonempty_list(rule) ; EOI ; { fun prog -> List.fold_left (fun acc r -> r acc) prog rules }

let rule :=
  | a=atom ; DOT ; { fun prog -> 
                      AbstractSyntax.Proto_Program.add_proto_rule (a,fun t -> [],t) prog}
     
  | head=atom ; FROM ; body=separated_nonempty_list(COMMA,atom) ; DOT ;
    { fun prog -> 
      AbstractSyntax.Proto_Program.add_proto_rule
        (head,
         fun tables ->
         let body_atoms,new_tables =
           List.fold_left
             (fun (preds,l_params) p ->
               let new_p,l_new_params = p l_params in
               new_p::preds,l_new_params)
             ([], tables)
             body in
         List.rev body_atoms,new_tables)
        prog }     


let parameters ==
  | params=separated_nonempty_list(COMMA,parameter) ;
           { fun tables -> 
             let parameters_rev,new_tables =
               List.fold_left
                 (fun (l_acc,l_tables) p ->
                   let new_p,new_tables=p l_tables in
                   new_p::l_acc,new_tables)
                 ([],tables)
                 params in
             let parameters,length =
               List.fold_left
                 (fun (acc,i) e -> e::acc,i+1)
                 ([],0)
                 parameters_rev in
             parameters,length,new_tables}
           
let atom :=
  | (sym,arity)=atom_sym ; LPAR ; params=parameters ; RPAR ;
    {fun (pred_id_table,tables) ->
     let parameters,length,new_tables=params tables in
     let () = check_arity sym length arity in
     let new_sym = Printf.sprintf "%s/%d" (fst sym) length in
     let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
     {AbstractSyntax.Predicate.p_id=pred_id;
      AbstractSyntax.Predicate.arity=length;
      AbstractSyntax.Predicate.arguments=parameters},(new_pred_id_table,new_tables) }

let atom_sym :=
 | sym=IDENT ; arity=arity? ; {sym,arity}

let arity :=
  | SLASH ; (arity,_arity_loc)=INT ; {arity}
    
     
let parameter :=
  | (cst,_loc)=INT ;
    {fun (var_table,const_table) -> 
     let cst,new_const_table=ConstGen.Table.add_sym (string_of_int cst) const_table in
     AbstractSyntax.Predicate.Const cst,(var_table,new_const_table)}
  | (var,_loc)=IDENT ;
    {fun (var_table,const_table) -> 
     let var,new_var_table=VarGen.Table.add_sym var var_table in
     AbstractSyntax.Predicate.Var var,(new_var_table,const_table)}
     
let query:=
  | ((pred_sym,loc),arity)=atom_sym ; LPAR ; params=parameters ; RPAR ; QUESTION_MARK ;
    { fun (pred_id_table,const_table) -> 
      let parameters,length,(_,new_const_table)=params (VarGen.Table.empty,const_table) in
      let () = check_arity (pred_sym,loc) length arity in
      let new_sym = Printf.sprintf "%s/%d" pred_sym length in
      let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
      {AbstractSyntax.Predicate.p_id=pred_id;
       AbstractSyntax.Predicate.arity=length;
       AbstractSyntax.Predicate.arguments=parameters},
      new_pred_id_table,new_const_table}


let extensional_facts :=
  | facts = nonempty_list(extensional_fact) ; EOI ;
    { fun (pred_id_table,cst_table,gen) ->
      List.fold_left
        (fun (l_acc,l_cst_tble,l_gen) f ->
          let r,new_cst_tble,new_gen = f (pred_id_table,l_cst_tble,l_gen) in
          r::l_acc,new_cst_tble,new_gen)
        ([],cst_table,gen)
      facts }
     
let extensional_fact :=
 |  ((pred_sym,loc),arity)=atom_sym ; LPAR ; params=parameters ; RPAR ; DOT ;
    { fun (pred_id_table,const_table,rule_id_gen) -> 
      let parameters,length,(_,new_const_table)= params (VarGen.Table.empty,const_table) in
      let () = check_arity (pred_sym,loc) length arity in
      let new_sym = Printf.sprintf "%s/%d" pred_sym length in
      try
        let pred_id = AbstractSyntax.Predicate.PredIdTable.find_id_of_sym new_sym pred_id_table in
        let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id rule_id_gen in
        let lhs = {AbstractSyntax.Predicate.p_id=pred_id;
		   AbstractSyntax.Predicate.arity=length;
		   AbstractSyntax.Predicate.arguments=parameters} in
        AbstractSyntax.Rule.({id=rule_id;
			      lhs=lhs;
			      e_rhs=[];
			      i_rhs=[];
			      i_rhs_num=0}),
        new_const_table,new_rule_id_gen
      with
      |  AbstractSyntax.Predicate.PredIdTable.Not_found -> 
          let () = flush stdout in
          let () = Printf.fprintf stderr "You try to add a fact about a predicate \"%s\" that is not a predicate of the program yet\n%!" new_sym in
          raise Parsing.Parse_error}
    
    
    



     
%%
