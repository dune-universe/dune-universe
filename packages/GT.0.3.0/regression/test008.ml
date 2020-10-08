@type ident = [`Var of string]

class ['r, 'self] ident_eval = object
  inherit [string -> 'r, 'self, 'r] @ident
  method c_Var s _ x = s x
end

@type 'a arith = [ `Add of 'a * 'a | `Sub of 'a * 'a]

class ['i, 'a, 'self] arith_eval fa = object
  inherit ['i, 'a, int, 'i, 'self, int] @arith
  method c_Add inh _ x y = (fa inh x) + (fa inh y)
  method c_Sub inh _ x y = (fa inh x) - (fa inh y)
end

@type 'a expr = [ ident | 'a arith ]

class ['a, 'self] expr_eval fa fself = object
  inherit [string->int, 'a, int, string->int, 'self, int] @expr
  inherit [int, 'self] ident_eval
  inherit [string -> int, 'a, 'self] arith_eval fa
end

let _ =
  let rec eval f x = GT.transform(expr) (new expr_eval eval) f x in
  Printf.printf "%d\n" @@
  eval (function "x" -> 1 | "y" -> 2) (`Add (`Var "x", `Var "y"))
