open Utility
open Types
open MParser
open MParser_PCRE
open Tokens

module String = Core.String

let make_group l = Final (Group (Group.of_char_list l))
let make_uletter l = Final (Letter (from_upper l))
let make_lletter l = Final (Letter (from_lower l))
let make_dot _ = Final Dot
let make_star _ = Final Star
let make_fit l = Expand (Fit l)

let make_fn s =
  match (String.lowercase s) with
  | "a" | "anagram" -> Anagram
  | "m" | "multi" -> Multi
  | "p" | "pattern" -> Pattern
  | "b" | "build" -> Build
  | "l" | "length" -> Length
  | "ab" | "above" -> Above
  | "be" | "below" -> Below
  | s -> Fn s

let make_bop s =
  match s with
  | "|" -> Union
  | "&" -> Inter
  | "-" -> Diff
  | s   -> Op s

let make_binary o l r = Bop ((make_bop o), l, r)

let make_var s = Var s
let make_unary s t = Fun (s, t)

let make_expr e = Expr e
let make_assign v e = Assign (v, e)
let make_implicit_expr s = Tiles s
let make_command s = Command (make_fn s)

(* parse arguments to anagram/pattern *)

let group = squares (many1 alphanum)
let fit = brackets (many1 (alphanum <|> dot))

let tile : (input_tile, unit) parser = (
      (group |>> make_group)
  <|> (fit |>> make_fit)
  <|> (dot |>> make_dot)
  <|> (char '*' |>> make_star)
  <|> (uppercase |>> make_uletter)
  <|> (lowercase |>> make_lletter))

let rack : (input_tile list, unit) parser = many1 tile

(* parse expressions *)

let arg_char : (char, unit) parser = alphanum <|> any_of ".*$_[]<>"
let arg : (string, unit) parser = many1_chars arg_char
let args : (string list, unit) parser = sep_by arg spaces1

let name : (string, unit) parser =
  pipe2 letter (many alphanum) (
    fun c cs -> String.of_char_list (c :: cs))

let fn : (fn, unit) parser = name |>> make_fn

let varname : (string, unit) parser = char '$' >> name
let var : (expr, unit) parser = varname |>> make_var

let unary : (expr, unit) parser =
  pipe2 fn (spaces1 >> args) make_unary

let expr : (expr, unit) parser = unary <|> var

let infix sym : (expr, unit) operator =
  Infix (spaces >> skip_string sym >> spaces >> return (make_binary sym),
         Assoc_none)

let operators =
  [
    [ infix "&";
      infix "|";
      infix "-";
    ];
  ]

let rec term s = (parens exp <|> expr) s
    and exp s = expression operators term s

let line_expr : (line, unit) parser = exp |>> make_expr

let lhs : (string, unit) parser = (varname << spaces << char '=' << spaces)

let assign : (line, unit) parser = pipe2 lhs expr make_assign

let command : (string, unit) parser =
        (char ';' >> name)
    <|> (letter |>> String.of_char)

let command_expr : (line, unit) parser = (command << eof) |>> make_command

let line : (line, unit) parser =
      (attempt assign)
  <|> (attempt line_expr)
  <|> (attempt command_expr)
  <|> (arg |>> make_implicit_expr)

let input : (line, unit) parser = line << eof

(* exported parsing functions *)

let make_parser fn =
  let parse s = match parse_string fn s () with
  | MParser.Success out -> Ok out
  | MParser.Failed (m, _) -> Error m
  in
  parse

let parse = make_parser input
let parse_rack = make_parser rack
let parse_int = make_parser integer
