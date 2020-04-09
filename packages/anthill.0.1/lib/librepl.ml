open Types
open Top

let new_env dict =
  { Env.dict = dict; op = Anagram; vars = Vars.empty }

(* Override the active operation when an explicit unary command is entered *)
let op_of_expr env expr = match expr with
| Expr (Fun (o, _)) -> begin
    match o with
    | Anagram | Pattern | Build -> o
    | _ -> env.Env.op
  end
| Command (Fn _) -> env.Env.op
| Command o -> o
| _ -> env.Env.op

let prompt_of_op = function
  | Anagram -> "anagram > "
  | Multi -> "multi > "
  | Pattern -> "pattern > "
  | Build -> "build > "
  | Fn s -> s ^ " > "
  | _ -> "other > "

