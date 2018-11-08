%{
open Bnf_spec.Bnf
open Spec
%}

%token LBRACK RBRACK DEF PIPE DOT EOF
%token <string> ID

%start start
%type <Bnf_spec.Bnf.grammar> start

%%

start
  : EOF { empty }
  | defs EOF { $1 }

defs
  : def { $1 }
  | defs def { union $1 $2 }

def : nt DEF prods DOT { List.fold_left (fun a -> add_prod a $1 ()) empty $3 }

prods
  : prod { [List.rev $1] }
  | prods PIPE prod { (List.rev $3) :: $1 }

prod
  : symbol { [$1] }
  | prod symbol { $2 :: $1 }

symbol
  : ID { T $1 }
  | nt { NT $1 }

nt : LBRACK ID RBRACK { $2 }
