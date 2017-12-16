%{
  open UtilsLib
  open IdGenerator
  open Datalog_AbstractSyntax

  let check_arity pred_sym length = function
    | Some a when a<>length -> 
      let () = flush stdout in
      let () = Printf.fprintf stderr "The specified arity of predicate '%s/%d' does not match the actual number of arguments (%d)\n%!" pred_sym a length in
     raise Parsing.Parse_error
    | _ -> ()
%}

%token <string> IDENT
%token <int> INT
%token LPAR RPAR COMMA DOT FROM EOI SLASH QUESTION_MARK


%start rule program extensional_facts query
%type < Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > rule
%type < Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t -> Datalog_AbstractSyntax.AbstractSyntax.Proto_Program.t > program
%type < (Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table) -> (Datalog_AbstractSyntax.AbstractSyntax.Predicate.predicate * Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table) > query
%type < (Datalog_AbstractSyntax.AbstractSyntax.Predicate.PredIdTable.table * Datalog_AbstractSyntax.ConstGen.Table.table*UtilsLib.IdGenerator.IntIdGen.t) -> (Datalog_AbstractSyntax.AbstractSyntax.Rule.rule list*Datalog_AbstractSyntax.ConstGen.Table.table*UtilsLib.IdGenerator.IntIdGen.t) > extensional_facts
   
%%
  
  program :
 | rule EOI { fun prog -> $1 prog }
 | rule program { fun prog ->
   let new_prog = $1 prog in
   $2 new_prog}
     
     rule :
 | predicate DOT { fun prog -> 
   AbstractSyntax.Proto_Program.add_proto_rule ($1,fun t -> [],t) prog}
     
 | predicate FROM predicate_list DOT { fun prog -> 
   AbstractSyntax.Proto_Program.add_proto_rule ($1,$3) prog }     
     
     predicate_list :
 | predicate {fun (pred_id_table,tables) -> 
   let predicate,(new_pred_id_table,new_tables)= $1 (pred_id_table,tables) in
   [predicate],(new_pred_id_table,new_tables) }
 | predicate COMMA predicate_list {fun (pred_id_table,tables) ->
   let predicate,(new_pred_id_table,new_tables)= $1 (pred_id_table,tables) in
   let remaining_pred,(new_pred_id_table',new_tables')=$3 (new_pred_id_table,new_tables) in
   predicate::remaining_pred,(new_pred_id_table',new_tables') }
     
     predicate :
 | pred_id LPAR parameters RPAR {fun (pred_id_table,tables) ->
   let pred_sym,arity = $1 in
   let parameters,new_tables=$3 tables in
   let length=List.length parameters in
   let () = check_arity pred_sym length arity in
   let new_sym = Printf.sprintf "%s/%d" pred_sym (List.length parameters) in
   let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
   {AbstractSyntax.Predicate.p_id=pred_id;
    AbstractSyntax.Predicate.arity=List.length parameters;
    AbstractSyntax.Predicate.arguments=parameters},(new_pred_id_table,new_tables) }

     pred_id :
 | IDENT SLASH INT {$1,Some $3}
 | IDENT {$1,None}
     
     parameters:
 | parameter {fun tables ->
   let par,new_tables=$1 tables in
   [par],new_tables}
 | parameter COMMA parameters {fun tables -> 
   let par,new_tables=$1 tables in
   let other_parameters,new_tables'=$3 new_tables in
   par::other_parameters,new_tables'}
     
     parameter :
 | INT {fun (var_table,const_table) -> 
   let cst,new_const_table=ConstGen.Table.add_sym (string_of_int $1) const_table in
   AbstractSyntax.Predicate.Const cst,(var_table,new_const_table)}
 | IDENT {fun (var_table,const_table) -> 
   let var,new_var_table=VarGen.Table.add_sym $1 var_table in
   AbstractSyntax.Predicate.Var var,(new_var_table,const_table)}
     
     query:
 |  pred_id LPAR parameters RPAR QUESTION_MARK { fun (pred_id_table,const_table) -> 
   let pred_sym,arity = $1 in
   let parameters,(_,new_const_table)=$3 (VarGen.Table.empty,const_table) in
   let length=List.length parameters in
   let () = check_arity pred_sym length arity in
   let new_sym = Printf.sprintf "%s/%d" pred_sym length in
   let pred_id,new_pred_id_table = AbstractSyntax.Predicate.PredIdTable.add_sym new_sym pred_id_table in
   {AbstractSyntax.Predicate.p_id=pred_id;
    AbstractSyntax.Predicate.arity=length;
    AbstractSyntax.Predicate.arguments=parameters},
   new_pred_id_table,new_const_table}
     





     extensional_facts :
 | extensional_fact EOI {fun param ->
   let r,new_cst_tble,new_gen = $1 param in
   [r],new_cst_tble,new_gen}
 | extensional_fact extensional_facts {fun (pred_id_table,cst_table,gen) ->
   let r_lst,new_cst_tble,new_gen = $2 (pred_id_table,cst_table,gen) in
   let r,new_cst_tble',new_gen' = $1 (pred_id_table,new_cst_tble,new_gen) in
   r::r_lst,new_cst_tble',new_gen'}
     
     extensional_fact :
 |  pred_id LPAR parameters RPAR DOT { fun (pred_id_table,const_table,rule_id_gen) -> 
   let pred_sym,arity = $1 in
   let parameters,(_,new_const_table)=$3 (VarGen.Table.empty,const_table) in
   let length=List.length parameters in
   let () = check_arity pred_sym length arity in
   let new_sym = Printf.sprintf "%s/%d" pred_sym length in
   try
     let pred_id = AbstractSyntax.Predicate.PredIdTable.find_id_of_sym new_sym pred_id_table in
     let rule_id,new_rule_id_gen=IntIdGen.get_fresh_id rule_id_gen in
     let lhs = {AbstractSyntax.Predicate.p_id=pred_id;
		AbstractSyntax.Predicate.arity=List.length parameters;
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
