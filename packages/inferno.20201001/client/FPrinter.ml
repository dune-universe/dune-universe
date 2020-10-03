(* A pretty-printer for F. *)

open PPrint
open F

(* -------------------------------------------------------------------------- *)

(* Types. *)

let print_tyvar x =
  OCaml.int x (* TEMPORARY *)

let rec print_type_aux level ty =
  assert (level >= 0);
  match ty with
  | TyVar x ->
      print_tyvar x
  | TyProduct tys ->
      surround_separate_map 2 0 (lbrace ^^ rbrace)
        lbrace star rbrace print_type tys
  | TyArrow (ty1, ty2) ->
      if level >= 2 then
        print_type_aux 1 ty1 ^^
        string " -> " ^^
        print_type ty2
      else
        parens (print_type ty)
  | TyForall (x, ty1) ->
      if level >= 3 then
        lbracket ^^
        print_tyvar x ^^
        rbracket ^^ space ^^
        print_type_aux 3 ty1
      else
        parens (print_type ty)
  | TyMu (x, ty1) ->
      if level >= 3 then
        string "mu "  ^^
        print_tyvar x ^^
        dot ^^ space ^^
        print_type_aux 3 ty1
      else
        parens (print_type ty)

and print_type ty =
  print_type_aux 3 ty

(* -------------------------------------------------------------------------- *)

(* Terms. *)

let print_tuple print_elem elems =
  let contents =
    match elems with
    | [elem] ->
        (* For arity-1 tuples we print (foo,)
           instead of (foo) which would be ambiguous. *)
        print_elem elem ^^ comma
    | _ ->
        separate_map (comma ^^ space) print_elem elems in
  surround 2 0 lparen contents rparen

let print_let_in lhs rhs body =
  string "let " ^^ lhs
  ^^ string " = " ^^ rhs
  ^^ string " in " ^^ body

let rec print_term_aux level t =
  assert (level >= 0);
  match t with
  | Var x ->
      string x
  | App (t1, t2) ->
      if level >= 1 then
        print_term_aux 1 t1 ^^
        space ^^
        print_term_aux 0 t2
      else
        parens (print_term t)
  | TyApp (t1, ty2) ->
      if level >= 1 then
        print_term_aux 1 t1 ^^
        space ^^ lbracket ^^
        print_type ty2 ^^
        rbracket
      else
        parens (print_term t)
  | Abs (x, ty1, t2) ->
      if level >= 2 then
        string "fun " ^^
        string x ^^
        string " : " ^^
        print_type ty1 ^^
        string " = " ^^
        print_term_aux 2 t2
      else
        parens (print_term t)
  | Let (x, t1, t2) ->
      if level >= 2 then
        print_let_in
          (string x)
          (print_term t1)
          (print_term_aux 2 t2)
      else
        parens (print_term t)
  | TyAbs (x, t1) ->
      if level >= 2 then
        string "FUN " ^^
        print_tyvar x ^^
        string " = " ^^
        print_term_aux 2 t1
      else
        parens (print_term t)
  | Tuple ts ->
      print_tuple print_term ts
  | Proj (i, t2) ->
      (* like [App] *)
      if level >= 1 then
        string "proj" ^^
        OCaml.int i ^^
        space ^^
        print_term_aux 0 t2
      else
        parens (print_term t)
  | LetProd (xs, t1, t2) ->
      if level >= 2 then
        print_let_in
          (print_tuple string xs)
          (print_term t1)
          (print_term_aux 2 t2)
      else
        parens (print_term t)
 
and print_term t =
  print_term_aux 2 t

