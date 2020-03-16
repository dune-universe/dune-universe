open Libnacc.Parsing
open Libnacc.Parsers

type expr =
  | Cst of float
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr
[@@deriving variants, show]

let rec parse_exp inp =
  inp
  --> (binop add '+' ~~parse_mul <|> binop sub '-' ~~parse_mul <|> ~~parse_mul)

and parse_mul inp =
  inp
  --> (binop mul '*' ~~parse_pow <|> binop div '/' ~~parse_pow <|> ~~parse_pow)

and parse_pow inp = inp --> (binop pow '^' ~~parse_fac <|> ~~parse_fac)

and parse_fac inp = inp --> (parenthesized '(' ~~parse_exp ')' <|> ~~parse_cst)

and parse_cst inp = inp --> (cst <$> floatingpoint)

(* let _ =
  do_parse ~~parse_exp "1+2*3"
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "(1+2)*3"
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "( 1 + 2 ) * ( 5 + 96) "
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "(1+2)*(5+96)"
  |> Option.get |> show_expr |> print_endline

let _ =
  do_parse ~~parse_exp "(1*2)+(5*96)"
  |> Option.get |> show_expr |> print_endline
 *)

let rec eval = function
  | Cst v -> v
  | Add (l, r) -> eval l +. eval r
  | Sub (l, r) -> eval l -. eval r
  | Mul (l, r) -> eval l *. eval r
  | Div (l, r) -> eval l /. eval r
  | Pow (l, r) -> Float.pow (eval l) (eval r)

let _ =
  while true do
    print_string "Calc # ";
    flush stdout;
    let fst (a, _) = a in
    try
      read_line () --> ~~parse_exp
      |> Option.get |> fst |> eval |> string_of_float |> print_endline
    with
    | Invalid_argument _ -> print_endline "Syntax error"
    | End_of_file ->
        print_endline "Bye.";
        exit 0
  done
