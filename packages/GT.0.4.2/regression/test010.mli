@type var = [`Var of string]
@type 'a lambda = [var | `Abs of string * 'a | `App of 'a * 'a] 
@type 'a var_expr = [var | `Num of int | `Add of 'a * 'a | `Mult of 'a * 'a] 
@type 'a expr = ['a lambda | 'a var_expr]
