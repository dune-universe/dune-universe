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
  | TyProduct (ty1, ty2) ->
      if level >= 1 then
        print_type_aux 0 ty1 ^^
        string " * " ^^
        print_type_aux 1 ty2
      else
        parens (print_type ty)
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
        string "let " ^^
        string x ^^
        string " = " ^^
        print_term t1 ^^
        string " in " ^^
        print_term_aux 2 t2
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
 | Pair (t1, t2) ->
      parens (
        print_term t1 ^^
        comma ^^ space ^^
        print_term t2
      )
  | Proj (i, t2) ->
      (* like [App] *)
      if level >= 1 then
        string "proj" ^^
        OCaml.int i ^^
        space ^^
        print_term_aux 0 t2
      else
        parens (print_term t)
 
and print_term t =
  print_term_aux 2 t

