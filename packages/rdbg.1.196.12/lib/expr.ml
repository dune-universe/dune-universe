(* Time-stamp: <modified the 27/08/2019 (at 16:00) by Erwan Jahier> *)

(* Expressions *)
type t = 
  | Op of oper * t list
  | True | False
  | Ival of Num.num | Fval of float 
  | Var of string
and 
 oper = 
  | And | Or | Xor | Impl | Not | Eq | Ite 
  | Sup | SupEq | Inf | InfEq  
  | Sum | Diff | Prod | Quot | Mod | Div | Uminus 
  | Call of string

let (to_string : t -> string) =
  fun t ->
    let rec aux b t =
      let b = "  "^b in
      (match t with
        | Op(And,[e1;e2]) -> (aux b e1)^" and " ^(aux b e2)
        | Op(Or,[e1;e2]) -> "("^(aux b e1)^" or " ^(aux b e2) ^ ")"
        | Op(Xor,[e1;e2]) -> "("^(aux b e1)^" xor"  ^(aux b e2) ^ ")"
        | Op(Impl,[e1;e2]) -> "("^(aux b e1)^" =>"  ^(aux b e2) ^ ")"
        | Op(Not,[e]) ->  "not("  ^(aux b e) ^")"
        | Op(Eq,[e1;e2]) -> (aux b e1)^ "="  ^(aux b e2)
      (*      | Op(Ite,[c;e1;e2]) -> "("^(aux b e1)^"ite"  ^(aux b e2) *)
        | Op(Sup,[e1;e2]) -> (aux b e1)^">"  ^(aux b e2) 
        | Op(SupEq,[e1;e2]) -> (aux b e1)^">="  ^(aux b e2)
        | Op(Inf,[e1;e2]) -> (aux b e1)^"<"  ^(aux b e2)
        | Op(InfEq,[e1;e2]) -> (aux b e1)^"<="   ^(aux b e2)
        | Op(Sum,[e1;e2]) -> "("^(aux b e1)^"+"  ^(aux b e2) ^ ")"
        | Op(Diff,[e1;e2]) -> "("^(aux b e1)^"-"  ^(aux b e2) ^ ")"
        | Op(Prod,[e1;e2]) -> (aux b e1)^"*" ^(aux b e2)
        | Op(Quot,[e1;e2]) -> (aux b e1)^"/"  ^(aux b e2)
        | Op(Mod,[e1;e2]) -> "("^(aux b e1)^"mod"  ^(aux b e2) ^ ")"
        | Op(Div,[e1;e2]) -> (aux b e1)^"/"  ^(aux b e2)

        | Op (op,l) -> (oper_aux b op) ^ "(\n"^b ^ 
          ((String.concat (",\n"^b) (List.map (aux b) l))^"\n"^b^")")
        | True -> "t"
        | False -> "f"
        | Ival(n) -> Num.string_of_num n
        | Fval(f) -> string_of_float f
        | Var(str) -> str
      )        
    and
        oper_aux _b = function  
          | And -> "and"
          | Or -> "or" 
          | Xor -> "xor" 
          | Impl -> "impl" 
          | Not -> "not" 
          | Eq -> "eq" 
          | Ite -> "ite" 
          | Sup -> "sup" 
          | SupEq -> "supeq" 
          | Inf -> "inf" 
          | InfEq -> "infeq"  
          | Sum -> "sum" 
          | Diff -> "diff" 
          | Prod -> "prod" 
          | Quot -> "quot" 
          | Mod -> "mod" 
          | Div -> "div" 
          | Uminus -> "uminus" 
          | Call str ->  str
    in
    aux "" t

let dump e = 
  print_string ((to_string e)^ "\n")

let rec (simplify : t -> t) =
  fun t ->
    match t with
      | Op(And, (Op(And,l)::tail)) -> simplify (Op(And, (l@tail)))
      | Op(Or, (Op(Or,l)::tail))   -> simplify (Op(Or,  (l@tail)))
      | Op(Sum, (Op(Sum,l)::tail)) -> simplify (Op(Sum, (l@tail)))
      | Op(Not,[Op(Not, [e])]) -> simplify e
      | Op(Not,[Op(Sup, [e1;e2])]) -> Op(InfEq, [simplify e1;simplify e2])
      | Op(Not,[Op(Inf, [e1;e2])]) -> Op(SupEq, [simplify e1;simplify e2])
      | Op(Not,[Op(SupEq, [e1;e2])]) -> Op(Inf, [simplify e1;simplify e2])
      | Op(Not,[Op(InfEq, [e1;e2])]) -> Op(Sup, [simplify e1;simplify e2])
      | Op (op,l) -> Op (op, List.map simplify l)
      | e -> e
