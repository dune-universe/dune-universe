[@@@ocaml.warning "-41"]

module Ast = struct
  type t =
    | Num of int
    | Var of string
    | BinOp of string * t * t
    | Seq of t list
    | IfThenElse of t * t * t option
    | TyAs of t * t
    | Tuple of t list
    | TyArrow of t * t
    | TyTuple of t list
    | Uminus of t
    | App of t * t
end

module OTree = struct
  type t =
    | Parens of t
    | Num of int
    | Var of string
    | BinOp of string * t * t
    | Seq of t list
    | TyAs of t * t
    | Tuple of t list
    | TyArrow of t * t
    | TyTuple of t list
    | IfThenElse of t * t * t option
    | Uminus of t
    | App of t * t

  let rec print = function
    | Parens t -> "(" ^ print t ^ ")"
    | Num n -> string_of_int n
    | Var n -> n
    | BinOp (n, t1, t2) -> print t1 ^ " " ^ n ^ " " ^ print t2
    | Seq ts -> String.concat "; " (List.map print ts)
    | TyAs (t1, t2) -> print t1 ^ " as " ^ print t2
    | Tuple ts -> String.concat ", " (List.map print ts)
    | TyArrow (t1, t2) -> print t1 ^ " -> " ^ print t2
    | TyTuple ts -> String.concat " * " (List.map print ts)
    | IfThenElse (t1, t2, None) -> "if " ^ print t1 ^ " then " ^ print t2
    | IfThenElse (t1, t2, Some t3) -> "if " ^ print t1 ^ " then " ^ print t2 ^ " else " ^ print t3
    | Uminus t -> "-" ^ print t
    | App (t1, t2) -> print t1 ^ " " ^ print t2
end
  
open Parenthesize
module M = Parenthesize.Make(struct type t = OTree.t end)
open M

open OTree

let p x = Parens x

let rec render = function
  | Ast.Num n -> atom (OTree.Num n)
  | Ast.Var s -> atom (OTree.Var s)

(*
%nonassoc IN
%nonassoc below_SEMI
%nonassoc SEMI                          /* below EQUAL ({lbl=...; lbl=...}) */
*)

  | Ast.Seq ts ->
      list p (fun xs -> Seq xs) 0.25 (List.map render ts)

(*
%nonassoc LET                           /* above SEMI ( ...; let ... in ...) */
%nonassoc below_WITH
%nonassoc FUNCTION WITH                 /* below BAR  (match ... with ...) */
%nonassoc AND             /* above WITH (module rec A: SIG with ... and ...) */
%nonassoc THEN                          /* below ELSE (if ... then ...) */
%nonassoc ELSE                          /* (if ... then ... else ...) */
*)

  | Ast.IfThenElse (t1, t2, Some t3) ->
      list p (function
        | [x; y; z] -> IfThenElse (x, y, Some z)
        | _ -> assert false)
        0.5
        [reset (render t1); render t2; render t3]

  | Ast.IfThenElse (t1, t2, None) ->
      list p (function
        | [x; y] -> IfThenElse (x, y, None)
        | _ -> assert false)
        0.5
        [reset (render t1); render t2]
        
(*
%nonassoc LESSMINUS                     /* below COLONEQUAL (lbl <- x := e) */
%right    COLONEQUAL                    /* expr (e := e := e) */
%nonassoc AS
*)

  | Ast.TyAs (t1, t2) ->
      binop p (fun x y -> TyAs (x, y)) Noassoc 0.6 (render t1) (render t2) 

(*
%left     BAR                           /* pattern (p|p|p) */
%nonassoc below_COMMA
%left     COMMA                         /* expr/expr_comma_list (e,e,e) */
*)

  | Ast.Tuple ts ->
      list p (fun ts -> Tuple ts) 0.8 (List.map render ts)
(*
%right    MINUSGREATER                  /* core_type2 (t -> t -> t) */
*)

  | Ast.TyArrow (t1, t2) ->
      binop p (fun x y -> TyArrow (x, y)) Right 0.9 (render t1) (render t2)

(*
%right    OR BARBAR                     /* expr (e || e || e) */
%right    AMPERSAND AMPERAMPER          /* expr (e && e && e) */
%nonassoc below_EQUAL
%left     INFIXOP0 EQUAL LESS GREATER   /* expr (e OP e OP e) */
%right    INFIXOP1                      /* expr (e OP e OP e) */
%right    COLONCOLON                    /* expr (e :: e :: e) */
%left     INFIXOP2 PLUS PLUSDOT MINUS MINUSDOT  /* expr (e OP e OP e) */
%left     INFIXOP3 STAR                 /* expr (e OP e OP e) */
%right    INFIXOP4                      /* expr (e OP e OP e) */
*)

  | Ast.BinOp (("+" | "-" as op), t1, t2) ->
      binop p (fun x y -> BinOp (op, x, y)) Left 1.0
        (render t1)
        (render t2)
  | Ast.BinOp (("*" as op), t1, t2) ->
      binop p (fun x y -> BinOp (op, x, y)) Left 2.0
        (render t1)
        (render t2)
  | Ast.BinOp _ -> assert false

  | Ast.TyTuple ts ->
      list p (fun ts -> TyTuple ts) 2.0 (List.map render ts)

(*
%nonassoc prec_unary_minus prec_unary_plus /* unary - */
*)

  | Ast.Uminus t ->
      prefix p (fun x -> Uminus x) 5.0 (render t)
  
(*
%nonassoc prec_constant_constructor     /* cf. simple_expr (C versus C x) */
%nonassoc prec_constr_appl              /* above AS BAR COLONCOLON COMMA */
%nonassoc below_SHARP
%nonassoc SHARP                         /* simple_expr/toplevel_directive */
%nonassoc below_DOT
%nonassoc DOT
/* Finally, the first tokens of simple_expr are above everything else. */
%nonassoc BACKQUOTE BANG BEGIN CHAR FALSE FLOAT INT INT32 INT64
          LBRACE LBRACELESS LBRACKET LBRACKETBAR LIDENT LPAREN
          NEW NATIVEINT PREFIXOP STRING TRUE UIDENT
*)

  | Ast.App (t1, t2) ->
      binop p (fun x y -> App (x, y)) Left 100.0 (render t1) (render t2)
  
module Test = struct
  open Ast
  let num z = Num z
  let id x = Var x
  let int = id "int"
  let alpha = id "'a"

  let (+) x y = BinOp("+",x,y)
  let (-) x y = BinOp("-",x,y)
  let ( * ) x y = BinOp("*",x,y)
  let uminus x = Uminus x
  let (^->) x y = TyArrow(x,y)
  let ty_as x y = TyAs(x,y)
  let tuple xs = Tuple xs
  let app t1 t2 = App (t1, t2)
  let sequence ts = Seq ts
  let if_then_else t1 t2 t3 = IfThenElse(t1,t2,Some t3)
  let if_then t1 t2 = IfThenElse(t1,t2,None)
    
  let test t answer = 
    let str = OTree.print (render t Noassoc 0.0) in
    Format.eprintf "%s =?= %s@." str answer;
    if str <> answer then failwith "FAILED"
(*    Format.eprintf "%a@.@." Token.format  (t Noassoc 0.0);
    Format.eprintf "%a@.@." Token.dump  (t Noassoc 0.0)
*)

  let test () =
    test (num 0) "0";
    test (num 0 + num 0) "0 + 0";
    test (num 0 + num 0 * num 0) "0 + 0 * 0";
    test (num 0 * (num 0 + num 0)) "0 * (0 + 0)";
    test ((num 1 + num 2) * num 3) "(1 + 2) * 3";
    test (num 1 + (num 2 + num 3)) "1 + (2 + 3)"; (* It is as same as 1 + 2 + 3 but just semantically *)
    test (num 1 - (num 2 - num 3)) "1 - (2 - 3)";
    test (num 1 - num 2 - num 3) "1 - 2 - 3";
    test (uminus (num 1)) "-1";
    test (uminus (num 1 + num 2 + num 3))   "-(1 + 2 + 3)";
    test (num 1 + uminus (num 1)) "1 + -1";
(* CR jfuruse: FIXME
    test (uminus (uminus (num 1))) "- -1";
*)
    test (int ^-> int ^-> int) "int -> int -> int";
    test ((int ^-> int) ^-> int) "(int -> int) -> int";
    test (ty_as (int ^-> int ^-> int) alpha) "int -> int -> int as 'a";
    test ((ty_as (int ^-> int) alpha) ^-> int) "(int -> int as 'a) -> int";
    test (tuple [num 1; num 2; num 3]) "1, 2, 3";
    test (tuple [num 1; tuple [num 2; num 3]; num 4]) "1, (2, 3), 4";
    test (app (app (id "x") (id "y")) (id "z")) "x y z";
    test (app (id "x") (app (id "y") (id "z"))) "x (y z)";
    test (app (id "x") (num 1 * num 2)) "x (1 * 2)";
    test (sequence [ num 1 + num 2; num 1 + num 2; num 1 + num 2 ]) "1 + 2; 1 + 2; 1 + 2";
    test (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1 + num 2)) "if 1 + 2 then 1 + 2 else 1 + 2";
    test (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1) + num 2) "(if 1 + 2 then 1 + 2 else 1) + 2";
    test (app (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1)) (num 2)) "(if 1 + 2 then 1 + 2 else 1) 2";
    test (app (id "f") (if_then_else (num 1 + num 2) (num 1 + num 2) (num 1 + num 2))) "f (if 1 + 2 then 1 + 2 else 1 + 2)";
    test (if_then_else 
            (sequence [ num 1 + num 2; num 1 + num 2 ])
            (sequence [ num 1 + num 2; num 1 + num 2 ])
            (sequence [ num 1 + num 2; num 1 + num 2 ])) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2) else (1 + 2; 1 + 2)";
    test (sequence [if_then_else
                 (sequence [ num 1 + num 2; num 1 + num 2 ])
                 (sequence [ num 1 + num 2; num 1 + num 2 ])
                 (sequence [ num 1 + num 2; num 1 + num 2 ]);
               num 1 + num 2 ]) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2) else (1 + 2; 1 + 2); 1 + 2";
    test (if_then
            (sequence [ num 1 + num 2; num 1 + num 2 ])
            (sequence [ num 1 + num 2; num 1 + num 2 ])) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2)";
    test (sequence [if_then 
                 (sequence [ num 1 + num 2; num 1 + num 2 ])
                 (sequence [ num 1 + num 2; num 1 + num 2 ]);
               num 1 + num 2 ]) "if 1 + 2; 1 + 2 then (1 + 2; 1 + 2); 1 + 2";
    prerr_endline "done"

end
