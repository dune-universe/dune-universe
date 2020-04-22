open L;;

exception NotAChurchTerm;;

let rec iter f x n = if n=0 then x else App(f,(iter f x (n-1)));;

let of_int n = Abs("f",Abs("x",iter (Var "f") (Var "x") n));;

let rec to_int t = match t with
  Abs(_,Abs(x,t')) -> (
    match t' with 
      Var _ -> 0
    | App(Var f,s) -> 1 + to_int (Abs(f,Abs(x,s)))
    | _ -> raise NotAChurchTerm)
  | _ -> raise NotAChurchTerm
;;

let convert_to_church t = 
  let rec convin t = match t with
    App(Var f, inner) -> App(Var "f", convin inner)
  | App(Var f, Var x) -> App(Var "f", Var "x")
  | Var x -> Var "x"
  in match t with 
  | Abs (f, Abs (x, inner)) -> Abs("f", Abs("x", convin inner))
  | _ -> raise NotAChurchTerm
;;

let is_church t = try to_int t |> ignore; true with | _ -> false;;
