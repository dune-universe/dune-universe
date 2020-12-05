%token <Logic.Abstract_syntax.Abstract_syntax.location> ARROW
%token <Logic.Abstract_syntax.Abstract_syntax.location> LIN_ARROW
%on_error_reduce type_expression

%%

%public type_expression :
| ty = atomic_type_or_term {
             fun sg ->
             let id',loc = ty in
	     match Environment.Signature1.is_type id' sg with
	     | true -> Abstract_syntax.Type_atom (id',loc,[]),loc
	     | false -> emit_parse_error (Error.Unknown_type id') loc
         }
                           
| ty = not_atomic_type_expression { ty }

  
(*
%public type_expression :
| ty = elementary_type { ty }
| ty1 = elementary_type arrow = arrow ty2 = type_expression
    {
      fun s ->
      let ty1',loc1 = ty1 s in
      let ty2',loc1 = ty1 s in
      let new_loc = new_loc loc1 loc2 in
      match arrow with
      | Abstract_syntax.Linear -> Abstract_syntax.Linear_arrow (ty1,ty2,new_loc),new_loc
      | Abstract_syntax.Non_linear -> Abstract_syntax.Arrow (ty1,ty2,new_loc),new_loc
}
 *)
%inline arrow :
| ARROW { Abstract_syntax.Non_linear }
  | LIN_ARROW { Abstract_syntax.Linear }

              (*
elementary_type :
      | id = IDENT
               {
                 fun sg ->
                 let id',loc = id in
	         match Environment.Signature1.is_type id sg with
	         | true -> Abstract_syntax.Type_atom (id,l,[]),l
	         | false -> emit_parse_error (Error.Unknown_type id) loc
               }
| LPAREN ty = type_expression RPAREN { ty }
               *)        


  %public not_atomic_type_expression :
| ty = not_atomic_elementary_type { ty }
| ty1 = not_atomic_elementary_type arrow = arrow ty2 = type_expression
    {
      fun s ->
      let ty1',loc1 = ty1 s in
      let ty2',loc2 = ty2 s in
      let new_loc = new_loc loc1 loc2 in
      match arrow with
      | Abstract_syntax.Linear -> Abstract_syntax.Linear_arrow (ty1',ty2',new_loc),new_loc
      | Abstract_syntax.Non_linear -> Abstract_syntax.Arrow (ty1',ty2',new_loc),new_loc
}
| ty1 = atomic_type_or_term arrow = arrow ty2 = type_expression
    {
      fun s ->
      let ty1',loc1 = 
        let id,loc = ty1 in
	match Environment.Signature1.is_type id s with
	| true -> Abstract_syntax.Type_atom (id,loc,[]),loc
	| false -> emit_parse_error (Error.Unknown_type id) loc in      
      let ty2',loc2 = ty2 s in
      let new_loc = new_loc loc1 loc2 in
      match arrow with
      | Abstract_syntax.Linear -> Abstract_syntax.Linear_arrow (ty1',ty2',new_loc),new_loc
      | Abstract_syntax.Non_linear -> Abstract_syntax.Arrow (ty1',ty2',new_loc),new_loc
}


not_atomic_elementary_type :
| LPAREN ty = not_atomic_type_expression RPAREN { ty }

(*                                         
 atomic_elementary_type :
| id = IDENT
         {
           fun sg ->
           let id',loc = id in
	   match Environment.Signature1.is_type id sg with
	   | true -> Abstract_syntax.Type_atom (id,l,[]),l
	   | false -> emit_parse_error (Error.Unknown_type id) loc
         }
| LPAREN ty = atomic_elementary_type RPAREN { ty }
 *)
