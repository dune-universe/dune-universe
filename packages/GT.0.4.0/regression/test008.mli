@type ident = [`Var of string] 
@type 'a arith = [ `Add of 'a * 'a | `Sub of 'a * 'a] 
@type 'a expr = [ ident | 'a arith ] 

