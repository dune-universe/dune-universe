(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Sexplib.Conv

type ast =
  | Break
  | Ellipsis
  | Ident of string
  | Number of string
  | Bool of string
  | String of string
  | Args of ast
  | Assign of ast * ast
  | Binop of string * ast * ast
  | Do of ast
  | Elseif of ast * ast
  | Fassign of ast * ast
  | Fbody of ast * ast
  | For1 of ast * ast * ast * ast
  | For2 of ast * ast * ast * ast * ast
  | Forin of ast * ast * ast
  | Function of ast * ast
  | FunctionE of ast
  | Goto of ast
  | If1 of ast * ast
  | If2 of ast * ast * ast
  | If3 of ast * ast * ast
  | If4 of ast * ast * ast * ast
  | Key1 of ast
  | Key2 of ast
  | Label of ast
  | Lassign of ast * ast
  | Lfunction of ast * ast
  | Lnames of ast
  | Member of ast * ast
  | Mcall of ast * ast * ast
  | Pexp of ast
  | Repeat of ast * ast
  | Return of ast
  | Table of ast
  | Unop of string * ast
  | Vargs of ast
  | While of ast * ast
  | Clist of ast list
  | Elist of ast list
  | FNlist of ast list
  | Slist of ast list
[@@deriving sexp, show]

let extract_list = function
  | Clist i -> i
  | Elist i -> i
  | FNlist i -> i
  | Slist i -> i
  | _ -> failwith "Not a valid ast list!"

