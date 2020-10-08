open GT

@type 'expr a_expr = [
| `Var    of string
| `Const  of int
| `Binop  of string * 'expr * 'expr
| `Assign of string * 'expr
] with show, gmap, eval, stateful

@type expr = expr a_expr with show, gmap, eval, stateful
                 
let eval st e =
  fix0 (fun f st e ->
          match transform(a_expr) (new @a_expr[stateful] f f) st e with
          | st', `Var    x          -> st', st' x
          | st', `Const  y          -> st', y
          | st', `Assign (x, y)     -> (fun z -> if z = x then y else st' z), y
          | st', `Binop  (op, x, y) ->
               st',
               match op with
               | "+" -> x + y
               | "-" -> x - y
       ) st e

let _ =
  let e = `Binop ("+", `Binop ("-", `Const 10, `Var "x"), (`Binop ("+", `Var "a", `Const 3))) in
  let f = `Binop ("+", `Binop ("-", `Const 10, `Assign ("a", `Const 10)), (`Binop ("+", `Var "a", `Const 3))) in
  Printf.printf "Original: %s\n" (show(expr) e);
  Printf.printf "Value   : %d\n" (snd @@ eval (function "x" -> 3 | "a" -> 8) f)
  
 
class simplifier f =
  object inherit [_] @expr[gmap] f
    method c_Div _ x y =
      match f x, f y with
      | Const x, Const y -> Const (x / y)
      | x      , Const 1 -> x
      | x      , y       -> Div (x, y)
    method c_Mul _ x y =
      match f x, f y with
      | Const x, Const y        -> Const (x * y)
      | Const 0, _ | _, Const 0 -> Const 0
      | Const 1, y              -> y
      | x, Const 1              -> x
      | x, y                    -> Mul (x, y)
    method c_Add _ x y =
      match f x, f y with
      | Const x, Const y -> Const (x + y)
      | Const 0, y       -> y
      | x, Const 0       -> x
      | x, y             -> Add (x, y)
  end

class ns_simplifier f =
  object inherit simplifier f 
    method c_Mul _ x y =
      match f x with
      | Const 0 -> Const 0
      | Const 1 -> f y
      | Const x -> (match f y with                      
                    | Const y -> Const (x * y)
                    | y       -> Mul   (Const x, y)
                   )
      | x       -> (match f y with
                    | Const 0 -> Const 0
                    | Const 1 -> x
                    | y       -> Mul (x, y)
                   )
  end
 (*   
let fix f =
  let knot    = ref (fun _ -> assert false) in
  let recurse = ref (f !knot) in
  knot := recurse;
  fun x -> recurse x
  
(*
let recurse t = f !knot t in
knot := recurse;
recurse t
 *)                 
(*          
let rec fix f =
  let g = lazy (f (fix f)) in
  fun x -> (Lazy.force g) x 
*)
                   
let rec simplify e =
  fix
    (fun f -> transform(expr) (Printf.printf "new\n"; new simplifier f) ())
    e
                         
                         (*
  let sobj = lazy (transform(expr) (Printf.printf "new\n"; new simplifier simplify)) in
  fun e ->
    (Lazy.force sobj) () e 
                          *)
                          *)
    
let ns_simplify e = fix0 (fun f -> transform(expr) (new ns_simplifier f) ()) e

let substitute st e =
  fix0
    (fun f ->
       transform(expr)
         (object inherit [_] @expr[gmap] f
            method c_Var _ x = Const (st x)
          end)
         ()) e

 let eval st e = let Const n = simplify @@ substitute st e in n
                                                                
 let _ =
   let e = Mul (`Add (`Var "a", `Const 3), `Add (`Const 5, `Var "b")) in
   Printf.printf "Original           : %s\n" (show(expr) e);
   Printf.printf "Simplified         : %s\n" (show(expr) @@ simplify e);
   Printf.printf "Substitute         : %s\n" (show(expr) @@ substitute (function "a" -> 0 | "b" -> 1) e);
   Printf.printf "Substitute+simplify: %s\n" (show(expr) @@ simplify @@ substitute (function "a" -> 0 | "b" -> 1) e);
   Printf.printf "Eval               : %d\n" (eval  (function "a" -> 0 | "b" -> 1) e);
   let e = `Mul (`Mul (`Var "a", `Const 0), `Div (`Const 1, `Const 0)) in
   Printf.printf "Simplified         : %s\n" (show(expr) @@ ns_simplify e)
