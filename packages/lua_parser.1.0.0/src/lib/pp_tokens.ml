(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Tokens
open Sexp_pretty

let pp_tok_sexp tok =
  print_string (sexp_to_string (sexp_of_token tok))

let pp_tok_show tok = print_string (show_token tok)
