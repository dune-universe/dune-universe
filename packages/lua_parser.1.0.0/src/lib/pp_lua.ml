(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Ast

let rec repeat s n =
  match n with
  | 0 -> ""
  | 1 -> s
  | _ -> s ^ repeat s (n - 1)

let count = ref 0

let indent () = count := !count + 3

let dedent () = count := !count - 3

let ws () = repeat " " !count

let add_nl str =
  str ^ "\n" 

let rec pp = function
  | Break -> "break"
  | Bool str -> str
  | Ellipsis -> "..."
  | Ident str -> str
  | Number str -> str
  | String str -> str
  | Binop(str, ex1, ex2) -> pp ex1 ^ " " ^ str ^ " " ^ pp ex2
  | Unop(str, ex) -> str ^ pp ex
  | Args lex -> "(" ^ pp lex ^ ")"
  | Assign(ex1, ex2) -> pp ex1 ^ " = " ^ pp ex2
  | Do ex -> "do" ^ pp_block ex false
  | Elseif(ex1, ex2) -> "elseif " ^ pp ex1 ^ " then" ^ pp_block ex2 true
  | Fassign(ex1, ex2) -> "[" ^ pp ex1 ^ "]" ^ " = " ^ pp ex2
  | Fbody(ex1, ex2) -> "(" ^ pp ex1 ^ ")" ^ pp_block ex2 false
  | Forin(ex1, ex2, ex3) -> "for " ^ pp ex1 ^ " in " ^ pp ex2 ^ " do" ^ pp_block ex3 false
  | For1(ex1, ex2, ex3, ex4) -> pp_for1 ex1 ex2 ex3 ex4
  | For2(ex1, ex2, ex3, ex4, ex5) -> pp_for2 ex1 ex2 ex3 ex4 ex5
  | Function(ex1, ex2) -> "function " ^ pp ex1 ^ pp ex2
  | FunctionE ex -> "function" ^ pp ex
  | Goto ex -> "goto " ^ pp ex
  | If1(ex1, ex2) -> "if " ^ pp ex1 ^ " then" ^ pp_block ex2 false
  | If2(ex1, ex2, ex3) -> pp_if2 ex1 ex2 ex3
  | If3(ex1, ex2, ex3) -> pp_if3 ex1 ex2 ex3
  | If4(ex1, ex2, ex3, ex4) -> pp_if4 ex1 ex2 ex3 ex4
  | Key1 ex -> "[" ^ pp ex ^ "]"
  | Key2 ex -> "." ^ pp ex
  | Label ex1 -> "::" ^ pp ex1 ^ "::"
  | Lassign(ex1, ex2) -> "local " ^ pp ex1 ^ " = " ^ pp ex2
  | Lnames ex -> "local " ^ pp ex
  | Lfunction(ex1, ex2) -> "local function " ^ pp ex1 ^ pp ex2
  | Member(ex1, ex2) -> pp ex1 ^ ":" ^ pp ex2
  | Mcall(ex1, ex2, ex3) -> pp ex1 ^ ":" ^ pp ex2 ^ pp ex3
  | Pexp ex -> "(" ^ pp ex ^ ")"
  | Repeat(ex1, ex2) -> "repeat" ^ pp_block ex1 true ^ "until " ^ pp ex2
  | Return ex -> "return " ^ pp ex
  | Table ex -> "{" ^ pp ex ^ "}"
  | Vargs ex -> pp ex ^ ", ..."
  | While(ex1, ex2) -> "while " ^ pp ex1 ^ " do" ^ pp_block ex2 false
  | Clist [] -> ""
  | Clist lex -> pp_clist lex
  | Elist [] -> ""
  | Elist lex -> pp_elist lex
  | FNlist [] -> ""
  | FNlist lex -> pp_fnlist lex
  | Slist [] -> ""
  | Slist lex -> pp_slist lex
and pp_block stl rpt =
  indent();
  let str = "\n" ^ ws () in
  let lex = extract_list stl in
  let out =
    if List.length lex > 0 then 
      let plex = List.map add_nl (List.map pp lex) in
      String.concat (ws ()) plex
    else "\n"
  in
  dedent ();
  let tl =
    match rpt with
    | false -> "end"
    | true -> ""
  in
  str ^ out ^ ws () ^ tl
and pp_slist lex =
  let plex = List.map add_nl (List.map pp lex) in
  ws () ^ String.concat (ws ()) plex
and pp_elist lex =
  let plex = List.map pp lex in
  String.concat ", " plex
and pp_clist lex =
  let plex = List.map pp lex in
  String.concat "" plex
and pp_fnlist lex =
  let plex = List.map pp lex in
  String.concat "." plex
and pp_if2 ex1 ex2 ex3 =
  "if " ^ pp ex1 ^ " then" ^ pp_block ex2 true ^ ws () ^ "else" ^ pp_block ex3 false
and pp_if3 ex1 ex2 ex3 =
  "if " ^ pp ex1 ^ " then" ^ pp_block ex2 true ^ "\n" ^ pp ex3 ^ ws () ^ "end"
and pp_if4 ex1 ex2 ex3 ex4 =
  "if " ^ pp ex1 ^ " then" ^ pp_block ex2 true ^ "\n" ^ pp ex3 ^ ws () ^ "else" ^ pp_block ex4 false
and pp_for1 ex1 ex2 ex3 ex4 =
  "for " ^ pp ex1 ^ " = " ^ pp ex2 ^ ", " ^ pp ex3 ^ " do" ^ pp_block ex4 false
and pp_for2 ex1 ex2 ex3 ex4 ex5 =
  "for " ^ pp ex1 ^ " = " ^ pp ex2 ^ ", " ^ pp ex3 ^ ", " ^ pp ex4 ^ " do" ^ pp_block ex5 false

let pp_lua ast = print_string (pp ast)

let pp_lua_str ast = pp ast
