%{ open Term_sequence_parser
          


        %}

%token <string*(Lexing.position*Lexing.position)> IDENT SYMBOL
%token LAMBDA DOT LPAREN RPAREN
%token EOI
                                                         
                                                         
%start <Term_sequence_parser.term> main

%%

main:
| t = term1 EOI
           { t }

term0:
| id = IDENT
         { Term (Var (fst id)) }
| id = SYMBOL
         { let name = fst id in
           Op (name,Term_sequence_parser.(get_fixity name test_sig)) }
| LPAREN t = term1  RPAREN
                  { Term t }
                                                         
term1:
| terms = term0+
            { let token,stream=next terms in
              parse_sequence [] token stream}
| LAMBDA vars = IDENT+ DOT t = term1
                                 { (List.fold_left
                                      (fun acc (var,_) ->
                                        (fun t -> acc (Abs (var,t))))
                                      (fun x -> x)
                                      vars)
                                     t }
                                 
                                                        
