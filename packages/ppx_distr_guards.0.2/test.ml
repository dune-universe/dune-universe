type t = A of int | B of int

(* problem *)
(*
let f = function
  | A x, _ (* A 0, A 1 would match here and fail the guard *)
  | _, A x when x<>0 -> 1 (* warning 57 *)
  | _ -> 2
(* sadly no easy way to have the guard checked for every pattern: *)
let f = function
  | A x, _ when x<>0 -> 1
  | _, A x when x<>0 -> 1 (* if these were big expressions, we would need to pull out a function for each case to avoid duplication *)
  | _ -> 2
*)

(* attribute for pattern or guard *)
(*
let f = function
  (* attributes can be attached to pattern and expr but not when or case: *)
  (* (A x, _ | _, A x) [@when x<>0] -> 1 (* [ppx_attr_pattern.ml] problem: or patterns need to be in brackets, leading | becomes syntax error, compiles w/o ppx but then w/o the guard -> unacceptable *) *)
  (* | [%distr ? x | x | x when x<>0] -> 2 (* better: use extension nodes, but then it only works with ppx *) *)
  | A x, _ | _, A x when (x<>0) [@distr] -> 1 (* [ppx_attr_guard.ml] better: w/o ppx we only get w57, problem: guard expr needs to be in brackets, and we need to annotate every guard *)
  | _ -> 2
*)

(* extension for pattern matches *)
let f = function%distr (* [ppx_ext_expr.ml]: append extension to keyword, applies to all cases, no brackets, does not compile w/o ppx *)
  | A x, _ | _, A x when x<>0 -> 1
  | _ -> 2

let g x = match%distr x with (* test match with nested or-patterns *)
  | A x, _ | _, A x | B x, _ when x<>0 -> 1
  | _ -> 2

let h = function%distr (* do not transform nested matches that don't have the extension! *)
  | A x, _ | _, A x | B x, _ when x<>0 -> 1
  | x ->
    match x with
    | A x, _ | _, A x | B x, _ when x<>0 -> 1
    | _ -> 2

(* let () = print_endline ("Result: " ^ string_of_int (f (A 0, A 1))) *)
