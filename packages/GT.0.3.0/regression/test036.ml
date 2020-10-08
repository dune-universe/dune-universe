open GT

@type expr =
  | Sub of (expr * expr) | Add of expr * expr | Ident of string | Const of int
  with show, html, gmap, foldl, foldr, eq, compare

let etoHTML e =
  HTML.toHTML (
    HTML.html (
      HTML.ul (
        HTML.li (transform(expr) (new @expr[html]) () e)
      )
    )
  );;

@type str = {a : expr; b : expr} with html

let stoHTML e =
  HTML.toHTML (
    HTML.html (
      HTML.ul (
        HTML.li (transform(str) (new @str[html]) () e)
      )
    )
  )

let _ =
  Printf.printf "%s\n" (etoHTML (Add (Ident "b", Add (Sub (Ident "a", Ident "b"), Const 1))));
  Printf.printf "%s\n" (stoHTML {a=(Add (Ident "b", Add (Sub (Ident "a", Ident "b"), Const 1))); b = Ident "c"});
