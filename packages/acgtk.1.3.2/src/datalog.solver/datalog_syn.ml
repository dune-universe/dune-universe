module Datalog_syn = 
struct

  (*
    The grammar:
    RULES -> Ident LHS_ARGS | EMPTY
    LHS_ARGS -> LPAR LHS_ARGS_LIST
    LHS_ARGS_LIST -> Ident LHS_ARGS_STRUCT
    LHS_ARGS_STRUCT -> RPAR RHS | COMMA LHS_ARGS_LIST
    RHS -> SEP PREMISSES
    PREMISSES -> Ident  PRED_OR_EQ | EQ EQ_ARGS | NEQ EQ_ARGS
    PRED_OR_EQ -> LPAR RHS_ARGS_LIST | EQ EQ_STRUCT | NEQ EQ_STRUCT
    EQ_STRUCT -> Ident PREMISSES_STRUCT
    EQ_ARGS -> LPAR RHS_ARGS_LIST
    PREMISSES_STRUCT -> COMMA PREMISSES | PERIOD RULES
    RHS_ARGS_LIST -> Ident RHS_ARGS_STRUCT
    RHS_ARGS_STRUCT -> RPAR  PREMISSES_STRUCT | COMMA RHAS_ARGS_LIST
  *)

  open Lexicalizer 
    
  type parse_context =
      {
        mutable sign: Signature.signature; (*signature under construction*)
        mutable pos_before_token : pos; (*position right befor the present token*)
        mutable pos:   pos; (*position in the stream*)
        mutable variables:   ((int*bool) String_map.t); (*variables of a rule, it associates to the name its identifer and a boolean which specifies whether the variable has an occurrence in the right hand side of a clause*)
        mutable lhs:  Program.predicate option; (*possible current lhs*)
        mutable rhs:  Program.predicate list; (*rhs of the current rule*)
        mutable current_pred_name:  string; (*name of the current predicate*)
        mutable arguments:  int list; (*list of arguments of the current predicate*)
        mutable arity:  int; (*number of arguments of the current predicate*)
        mutable fresh:  int; (*the next fresh identifier for variables*)
        mutable clauses:  Program.clause list
      }


  let empty_parse_context () = 
    {
      sign = Signature.empty; 
      pos_before_token = first_pos;
      pos = first_pos; 
      variables = String_map.empty;
      lhs = None;
      rhs = [];
      current_pred_name = "";
      arguments = [];
      arity = 0;
      fresh = 0;
      clauses=[]
    }

  let parse_error parse_context message = 
    let P(line,col) = parse_context.pos in
    let P(line_b,col_b) = parse_context.pos_before_token in
    let col_string = 
        if col >col_b
        then
          (string_of_int col_b)^"-"^(string_of_int col)
        else string_of_int col
    in failwith ("Line: "^string_of_int(line)^", Col: "^col_string^". "^message)

  let get_predicate_id parse_context =
    let name = parse_context.current_pred_name in
      (try
          let (ar,id) = Signature.find_pred_of_name name parse_context.sign in
            if ar = parse_context.arity
            then id
            else parse_error parse_context
              ("The predicate "^name^" should have arity "^string_of_int (ar)^" but is used with arity "^string_of_int(parse_context.arity)^".")
        with Not_found->
          let (id,sign) = Signature.add_pred_get_id parse_context.arity name parse_context.sign in
            parse_context.sign <- sign;
            id
      )

  let set_current_pred name parse_context =
    parse_context.current_pred_name <- name
    
  let make_lhs parse_context = 
    (let id = get_predicate_id parse_context in
      parse_context.lhs <- Some(Program.Pred(id,List.rev parse_context.arguments));
      parse_context.arguments <- [];
      parse_context.arity <- 0
    )

  let add_lhs_arg var_name parse_context = 
    (try 
        let (id,_) = String_map.find var_name parse_context.variables in
          parse_context.arity <- parse_context.arity+1;
          parse_context.arguments <- (id::parse_context.arguments)
      with 
          Not_found ->
            let fresh = parse_context.fresh in
              parse_context.variables <- (String_map.add var_name (fresh,false) parse_context.variables);
              parse_context.arguments <- (fresh::parse_context.arguments);
              parse_context.arity <- parse_context.arity+1;
              parse_context.fresh <- fresh+1
    )

  let add_rhs_arg var_name parse_context = 
      (try 
          let (id,right) = String_map.find var_name parse_context.variables in
            parse_context.arguments <- (id::parse_context.arguments);
            parse_context.arity <- parse_context.arity+1;
            (if right
            then ()
            else 
                parse_context.variables <- (String_map.add var_name (id,true) parse_context.variables)
            )
        with 
            Not_found ->
              let fresh = parse_context.fresh in
                parse_context.variables <- (String_map.add var_name (fresh,true) parse_context.variables);
                parse_context.arguments <- (fresh::parse_context.arguments);
                parse_context.arity <- parse_context.arity+1;
                parse_context.fresh <- fresh+1
      )

  let make_rhs_pred parse_context = 
    let id = get_predicate_id parse_context in
      parse_context.rhs <- ((Program.Pred(id, List.rev parse_context.arguments))::parse_context.rhs);
      parse_context.arguments <- [];
      parse_context.arity <- 0
        
  let make_clause parse_context = 
    match parse_context.lhs with 
        Some pred ->
          parse_context.clauses <- ((Program.Cl(pred,List.rev parse_context.rhs))::parse_context.clauses);
          parse_context.variables <- String_map.empty;
          parse_context.lhs <- None;
          parse_context.rhs <- [];
          parse_context.current_pred_name <- "";
          parse_context.fresh <- 0;
      | None -> failwith "make_clause: no lhs for clause"

  let make_program parse_context =
    Program.Prog(parse_context.sign,List.rev parse_context.clauses)
      
 
      
  let parse stream =
    let parse_context = empty_parse_context() in
    let get_token ()= 
      let tk = next_token stream parse_context.pos  in
        (match tk with
            Some(Ident(_,pb) as res,p)
          | Some(Str(_,pb) as res,p) 
          | Some(INIT(pb) as res,p)
          | Some(EQ(pb) as res,p) 
          | Some(NEQ(pb) as res,p)
          | Some(SEP(pb) as res,p) 
          | Some(LPAR(pb) as res,p)
          | Some(RPAR(pb) as res,p)
          | Some(COMMA(pb) as res,p)
          | Some(PERIOD(pb) as res,p)
          | Some(SEMICOL(pb) as res,p) -> 
              parse_context.pos_before_token <- pb;
              parse_context.pos <- p;
              Some res
          | None -> 
              parse_context.pos_before_token <-parse_context.pos;
              None
        );
    in
    let rec rules ()= 
      (match get_token () with
          Some(Ident(name,_)) ->
            set_current_pred name parse_context;
            lhs_args ()
        | None -> make_program parse_context
        |_ -> parse_error parse_context "Identifier expected."
      )
    and lhs_args () = 
      (match get_token () with
          Some(LPAR(_)) -> lhs_args_list()
        | _ -> parse_error parse_context "'(' expected."
      )
    and lhs_args_list () =
      (match get_token () with
          Some(Ident(name,_)) ->
            add_lhs_arg name parse_context;
            lhs_args_struct()
        |_ -> parse_error parse_context "Identifier expected."
      )
    and lhs_args_struct () =
      (match get_token() with
          Some(RPAR(_)) -> 
            make_lhs parse_context;
            rhs()
        | Some(COMMA(_)) ->
            lhs_args_list()
        |_ -> parse_error parse_context "',' or ')' expected."
      )
    and rhs () =
      (match get_token() with
          Some(SEP(_)) -> premisses()
        |_ -> parse_error parse_context "':-' expected."
      )
    and premisses ()=
      (match get_token() with
          Some(Ident(name,_)) -> pred_or_eq name
        | Some(EQ(_)) -> 
            set_current_pred "=" parse_context;
            parse_context.sign <- (Signature.add_eq parse_context.sign);
            eq_args ()
        | Some(NEQ(_)) ->
            set_current_pred "~=" parse_context;
            parse_context.sign <- (Signature.add_neq parse_context.sign);
            eq_args ()
        |_ -> parse_error parse_context "Identifier, '=', or '~=' expected."
      )
    and pred_or_eq name =
      (match get_token() with
          Some(LPAR(_)) -> 
            set_current_pred name parse_context;
            rhs_args_list()
        | Some(EQ(_)) ->
            set_current_pred "=" parse_context;
            parse_context.sign <- (Signature.add_eq parse_context.sign);
            add_rhs_arg name parse_context;
            eq_struct()
        | Some(NEQ(_)) ->
            set_current_pred "~=" parse_context;
            parse_context.sign <- (Signature.add_neq parse_context.sign);
            eq_struct()
        |_ -> parse_error parse_context "'(' or '=' expected."
      )
    and eq_struct () =
      (match get_token() with
          Some(Ident(name,_)) ->
            add_rhs_arg name parse_context;
            make_rhs_pred parse_context;
            premisses_struct()
        |_ -> parse_error parse_context "Identifier expected."
      )
    and eq_args () =
      (match get_token() with
          Some(LPAR(_)) -> rhs_args_list()
        |_ -> parse_error parse_context "'(' expected."
      )
    and premisses_struct ()=
      (match get_token() with
          Some(COMMA(_)) -> premisses ()
        | Some(PERIOD(_)) -> 
            make_clause parse_context;
            rules ()
        |_ -> parse_error parse_context "',' or ',' expected."
      )
    and rhs_args_list () =
      (match get_token() with
          Some(Ident(name,_)) ->
            add_rhs_arg name parse_context;
            rhs_args_struct()
        |_ -> parse_error parse_context "Identifier expected."
      )
    and rhs_args_struct () =
      (match get_token() with
          Some(RPAR(_)) ->
            make_rhs_pred parse_context;
            premisses_struct();
        | Some(COMMA(_)) ->
            rhs_args_list()
        |_ -> parse_error parse_context "')' or ',' expected."
      )
    in rules ()

end
