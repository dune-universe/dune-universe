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

let parse_mul = spaced (char '*') *> pure mul

let parse_add = spaced (char '+') *> pure add

let parse_sub = spaced (char '-') *> pure sub

let parse_div = spaced (char '/') *> pure div

let parse_pow = spaced (char '^') *> pure pow

let parse_cst = cst <$> floatingpoint

let parens p = parenthesized '(' p ')'

let expr =
  let rec parse_expr inp = inp --> chainl (parse_add <|> parse_sub) ~~parse_term
  and parse_term inp = inp --> chainl (parse_mul <|> parse_div) ~~parse_fact
  and parse_fact inp = inp --> chainr ~~parse_atom parse_pow
  and parse_atom inp = inp --> (parens ~~parse_expr <|> parse_cst) in
  ~~parse_expr


let rec eval = function
  | Cst v -> v
  | Add (l, r) -> eval l +. eval r
  | Sub (l, r) -> eval l -. eval r
  | Mul (l, r) -> eval l *. eval r
  | Div (l, r) -> eval l /. eval r
  | Pow (l, r) -> Float.pow (eval l) (eval r)


let process out =
  match out with
  | Ok expr -> print_float (eval expr) |> print_newline
  | Error e -> report e


let _ =
  while true do
    print_string "Calc # " ;
    flush stdout ;
    try do_parse expr (read_line ()) |> process with
    | Invalid_argument _ -> print_endline "Syntax error"
    | End_of_file ->
      print_endline "Bye." ;
      exit 0
  done
