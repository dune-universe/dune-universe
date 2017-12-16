open Datalog_signature
open String_map
open Pmcfg

module PMCFG_syn = 
struct
(*
The grammar:
GRAM -> INIT EQ Ident SEMICOL RULES
RULES -> Ident LHS_ARG | EMPTY
LHS_ARG -> LPAR LHS_ARG_LIST
LHS_ARG_LIST -> Ident LHS_ARG_STRUCT | String LHS_ARG_STRUCT
LHS_ARG_STRUCT -> Ident LHS_ARG_STRUCT | String LHS_ARG_STRUCT | COMMA LHS_ARG_LIST | RPAR RHS
RHS -> SEP PREMISSES | SEMICOL RULES
PREMISSES -> Ident RHS_ARG | SEMICOL RULES
RHS_ARG -> LPAR RHS_ARG_LIST
RHS_ARG_LIST -> Ident F_RHS_ARG_LIST
F_RHS_ARG_LIST -> COMMA RHS_ARG_LIST | RPAR PREMISSES
*)

open Lexicalizer.Lexicalizer

(*The parser*)

  type parse_context = 
      Parse of
        Datalog_signature.signature * (*signature under construction*)
          int * (*identifier of the start predicate*)
          pos * (*position in the stream*)
          ((int* bool) String_map.t) * (*variables of a rule, it associates to the name of the variable its identifier (an int), a boolean which sepcifies whether the variable has an occurrence that has been processed in the right hand side of the current rule*)
          PMCFG.lhs_cat option * (*possible current lhs*)
          PMCFG.rhs_cat list * (* rhs of the current rule *)
          string * (* name of the current predicate *)
          (PMCFG.argument list) * (* current lhs argument *)
          (PMCFG.argument list) list * (* list of arguments of a lhs predicate*)
          int list * (* list of arguments of a rhs predicate *)
          int * (*number of arguments of the current predicate*)
          int (*the next fresh identifier for variables*) *
          (PMCFG.grule list)
          

  let empty_parse_context = 
    Parse (Datalog_signature.empty,0,first_pos,String_map.empty,None,[],"",[],[],[],0,0,[])

  let parse_error pos message = 
    let P(line,col) = pos in
      failwith ("Line: "^string_of_int(line)^", Col: "^string_of_int(col)^". "^message)

  let parse_error_context tk parse_context message = 
     let Parse(_,_,pos,_,_,_,_,_,_,_,_,_,_) = 
      parse_context in
       match tk with
           Some(Ident(_,p))
         | Some(Str(_,p)) 
         | Some(INIT(p))
         | Some(EQ(p)) 
         | Some(NEQ(p))
         | Some(SEP(p)) 
         | Some(LPAR(p))
         | Some(RPAR(p))
         | Some(COMMA(p))
         | Some(PERIOD(p))
         | Some(SEMICOL(p)) -> parse_error p message
         | None -> parse_error pos message

  let set_init_id id parse_context = 
     let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
       Parse(sign,id,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules)

  let get_predicate_id ar name parse_context =
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
      (try
          let (ar0,id) = Datalog_signature.find_pred_of_name name sign in
            if ar = ar0
            then (parse_context,id)
            else parse_error pos 
              ("The predicate "^name^" should have arity "^string_of_int (ar0)^" but is used with arity "^string_of_int(ar)^".")
        with Not_found ->
          let (id,sign) = Datalog_signature.add_pred_get_id ar name sign in
          let parse_context =  
            Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) 
          in
            (parse_context,id)
      )

  let set_current_pred string parse_context = 
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
      Parse(sign,start,pos,variables,lhs,rhs,string,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules)

  let make_lhs parse_context = 
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
    let (parse_context,id) = get_predicate_id arity pred parse_context in
    let lhs = Some(PMCFG.Lhs_cat(id,List.rev lhs_arg)) in
    let Parse(sign,start,pos,variables,_,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context
    in  
      Parse(sign,start,pos,variables,lhs,rhs,pred,[],[],rhs_arg,0,fresh,rules)

            
  let add_lhs_arg_var var_name parse_context =
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
      (try 
          let (id,_) = String_map.find var_name variables in
          let top_lhs_arg = (PMCFG.Var id) :: top_lhs_arg in
            Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules)
        with
            Not_found -> 
              let variables = String_map.add var_name (fresh,false) variables in
              let top_lhs_arg = (PMCFG.Var fresh) :: top_lhs_arg in
                Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh+1,rules)            
      )
        
  let add_lhs_arg_string string parse_context = 
     let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
       let top_lhs_arg = (PMCFG.Val string) ::top_lhs_arg in
            Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules)

  let add_lhs_arg parse_context = 
     let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
     let lhs_arg = (List.rev top_lhs_arg)::lhs_arg in
       Parse(sign,start,pos,variables,lhs,rhs,pred,[],lhs_arg,rhs_arg,arity+1,fresh,rules)
              
  let add_rhs_arg var_name parse_context = 
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
      (try 
          let (id,right) = String_map.find var_name variables in
          let variables = 
            if right
            then parse_error pos ("Each variable may occurre at most once in the right hand side, but the variable "^var_name^" has at least two occurrences in the right hand side.")
            else String_map.add var_name (id,true) variables
          in
          let rhs_arg = id ::rhs_arg in
            Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity+1,fresh,rules)
        with
            Not_found -> 
              let variables = String_map.add var_name (fresh,true) variables in
              let rhs_arg = fresh :: rhs_arg in
            Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity+1,fresh+1,rules)
      )

  let add_rhs_pred parse_context = 
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
    let (parse_context,id) = get_predicate_id arity pred parse_context in
    let rhs=(PMCFG.Rhs_cat (id, List.rev rhs_arg))::rhs in
      let Parse(sign,start,pos,variables,lhs,_,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context
      in
      Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,[],0,fresh,rules)

  let add_rule parse_context = 
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
    let _ = 
      String_map.fold
        (function name ->
            function (id,right) ->
              function () ->
          if right
          then ()
          else parse_error pos ("The variable "^name^" is used in the left hand side of the rule but is not declared in the right hand side.")
        )
        variables
    in
      match lhs with
          Some lhs_cat ->
            let new_rule = PMCFG.R(lhs_cat,List.rev rhs) in
              Parse(sign,start,pos,String_map.empty,None,[],pred,[],[],[],0,0,new_rule::rules)
        | None -> failwith "impossible"
            
  let make_grammar parse_context = 
    let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
      PMCFG.Grammar(sign,rules,start)

  let next_token_context stream parse_context = 
     let Parse(sign,start,pos,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules) = 
      parse_context in
     let token = next_token stream pos in
       match token with 
           Some(tk,p) ->
             (Some(tk),Parse(sign,start,p,variables,lhs,rhs,pred,top_lhs_arg,lhs_arg,rhs_arg,arity,fresh,rules))
         | None-> (None,parse_context)
 
  
        
  let rec rules stream parse_context= 
      let (ident,parse_context) = next_token_context stream parse_context in
        (match ident with
            Some(Ident(s,_)) -> 
              let parse_context = set_current_pred s parse_context in
                lhs_arg stream parse_context
          | None -> make_grammar parse_context
          |_ -> parse_error_context ident parse_context "Identifier expected."
        )
  and lhs_arg stream parse_context = 
    let (lpar,parse_context) = next_token_context stream parse_context in
      (match lpar with
          Some(LPAR(_)) -> lhs_arg_list stream parse_context
        |_ -> parse_error_context lpar parse_context "'(' expected."
      )
  and lhs_arg_list stream parse_context =
    let (arg,parse_context) = next_token_context stream parse_context in
      (match arg with
          Some(Ident(s,_)) ->
            let parse_context = add_lhs_arg_var s parse_context in
              lhs_arg_struct stream parse_context
        | Some(Str(s,_)) ->
            let parse_context = add_lhs_arg_string s parse_context in
              lhs_arg_struct stream parse_context
        |_ -> parse_error_context arg parse_context "Identifier or string expected."
      )
  and lhs_arg_struct stream parse_context = 
    let (arg,parse_context) = next_token_context stream parse_context in
      (match arg with
          Some(Ident(s,_)) ->
            let parse_context = add_lhs_arg_var s parse_context in
              lhs_arg_struct stream parse_context
        | Some(Str(s,_)) ->
            let parse_context = add_lhs_arg_string s parse_context in
              lhs_arg_struct stream parse_context
        | Some(COMMA(_)) -> 
            let parse_context = add_lhs_arg parse_context in
              lhs_arg_list stream  parse_context
        | Some(RPAR(_)) -> 
            let parse_context = add_lhs_arg parse_context in
            let parse_context = make_lhs parse_context in
              rhs stream parse_context
        |_ -> parse_error_context arg parse_context "Identifier, string, ',' or ')' expected."
      )
  and rhs stream parse_context =
    let (tk,parse_context) = next_token_context stream parse_context in
      (match tk with
          Some(SEP(_)) ->
            premisses stream parse_context
        | Some(SEMICOL(_)) ->
            let parse_context = add_rule parse_context in
              rules stream parse_context
        |_ -> parse_error_context tk parse_context "':-' or ';' expected."
      )
  and premisses stream parse_context =
    let (ident,parse_context) = next_token_context stream parse_context in
      (match ident with
          Some(Ident(s,_)) ->
             let parse_context = set_current_pred s parse_context in
               rhs_arg stream parse_context
        | Some(SEMICOL(_)) ->
            let parse_context = add_rule parse_context in
              rules stream parse_context
        |_ -> parse_error_context ident parse_context "Identifier or ';' expected."
      )
  and rhs_arg stream parse_context = 
    let (lpar,parse_context) = next_token_context stream parse_context in
      (match lpar with
          Some(LPAR(_)) ->
            rhs_arg_list stream parse_context
        |_ -> parse_error_context lpar parse_context "'(' expected."
      )
  and rhs_arg_list stream parse_context = 
    let (ident,parse_context) = next_token_context stream parse_context in
      (match ident with
          Some(Ident(s,_)) ->
            let parse_context = add_rhs_arg s parse_context in
              f_rhs_arg_list stream parse_context
        |_ -> parse_error_context ident parse_context "Identifier expected."
      )
  and f_rhs_arg_list stream parse_context =
    let (tk,parse_context) = next_token_context stream parse_context in
      (match tk with
          Some(COMMA(_))-> rhs_arg_list stream parse_context
        | Some(RPAR(_)) -> 
            let parse_context = add_rhs_pred parse_context in
              premisses stream parse_context
        | _ -> parse_error_context tk parse_context "',' or ')' expected."
      )

  let parse stream =
    let parse_context = empty_parse_context in
    let (init,parse_context) = next_token_context stream parse_context in
      (match init with
          Some(INIT(_))->
            let (eq,parse_context) = next_token_context stream parse_context in
              (match eq with
                  Some(EQ(_)) ->
                    let (start_symb,parse_context) = next_token_context stream parse_context in
                      (match start_symb with
                          Some(Ident(s,_)) ->
                            let (parse_context,id) = get_predicate_id 1 s parse_context in
                            let parse_context = set_init_id id parse_context in
                            let (end_mark,parse_context) = next_token_context stream parse_context in
                              (match end_mark with
                                  Some(SEMICOL(_)) ->
                                    rules stream parse_context
                                |_ -> parse_error_context end_mark parse_context "';' expected."
                              )
                        |_ -> parse_error_context start_symb parse_context "Identifier expected."
                      )
                |_ -> parse_error_context eq parse_context "'=' expected."
              )
        |_ -> parse_error_context init parse_context "'INIT' expected."
      )
end 
