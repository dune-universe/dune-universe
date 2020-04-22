open L;;

exception NotABoolTerm;;

let ltrue = Abs("x",Abs("y",Var "x"));; 
let lfalse = Abs("x",Abs("y",Var "y"));;

let of_bool b = match b with
| true -> ltrue
| false -> lfalse
;;

let to_bool t =
  match t with 
  | Abs("x", Abs("y", Var "x")) -> true
  | Abs("x", Abs("y", Var "y")) -> false
  | _ -> raise NotABoolTerm
;;

let is_bool t = try to_bool t |> ignore; true with _ -> false;;