let rec to_string = function
| `Var   s     -> s
| `Abs  (s, x) -> "(\\" ^ s ^ " -> " ^ to_string x ^ ")"
| `App  (x, y) -> "(" ^ to_string x ^ " " ^ to_string y ^ ")"
| `Num   i     -> string_of_int i
| `Add  (x, y) -> "(" ^ to_string x ^ " + " ^ to_string y ^ ")"
| `Mult (x, y) -> "(" ^ to_string x ^ " * " ^ to_string y ^ ")"

let gensym = let n = ref 0 in fun () -> incr n; "_" ^ string_of_int !n;;

@type var = [`Var of string]

class ['self, 'v] var_eval = object
  inherit [(string * 'v) list, 'self, 'v] @var
  constraint 'self = [> var]
  method c_Var s _ name = try List.assoc name s with Not_found -> `Var name 
end

@type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a] 

class ['self] lambda_eval fa _fself = object
  inherit [ (string * 'self) list,  'self, 'self
          , (string * 'self) list,  'self, 'self
          ] @lambda
  inherit ['self, 'self] var_eval
  constraint 'self = [> 'self lambda ]
  method c_Abs env _ name l1 =
    let s' = gensym () in
    `Abs (s', fa ((name, `Var s')::env) l1)

  method c_App s _ l1 l2 =
    let l2' = fa s l2 in
    match fa s l1 with
    | `Abs (s, body) -> fa [s, l2'] body (* Why we don't extend old env here *)
    | l1'            -> `App (l1', l2')
 end

(* let (_: _ -> int) = new lambda_eval *)

let rec eval1 s e = GT.transform(lambda) (new lambda_eval eval1) s e;;

@type 'a var_expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a] 

class [ 'self ] var_expr_eval fa _fself = object
  inherit [ (string * 'self) list, 'self, 'self
          , (string * 'self) list, 'self, 'self
          ] @var_expr
  inherit ['self, 'self] var_eval
  constraint 'self = [> 'self var_expr ]

  method c_Num  _ _ i   = `Num i
  method c_Add  env _ x y =
    match fa env x, fa env y with
    | `Num x, `Num y -> `Num (x+y)
    | x, y           -> `Add (x, y)
  method c_Mult env _ x y =
    match fa env x, fa env y with
    | `Num x, `Num y -> `Num (x*y)
    | x, y           -> `Mult (x, y)
 end

let rec eval2 s e = GT.transform(var_expr)  (new var_expr_eval eval2) s e;;

@type 'a expr = ['a lambda | 'a var_expr]

class ['a, 'self] expr_eval fself = object
  inherit [ (string * 'self) list, 'a,    'self
          , (string * 'self) list, 'self, 'self
          ] @expr
  inherit ['self]   lambda_eval fself fself
  inherit ['self] var_expr_eval fself fself
  constraint 'self = [> 'a var_expr | 'a lambda ]
end 

let eval3 s e = GT.transform(expr) (new expr_eval) s e

let _ =
  Printf.printf "%s\n" @@
  to_string (eval3 ["x", `Num 5; "y", `Num 6] (`Add (`Var "x", `Mult (`Num 2, `Var "y"))))
