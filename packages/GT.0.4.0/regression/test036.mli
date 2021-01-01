open GT

@type expr =
   | Sub of (expr * expr) | Add of expr * expr | Ident of string | Const of int
   with show, html, gmap, foldl, foldr, eq, compare

@type str  = {a : expr; b : expr} with html
