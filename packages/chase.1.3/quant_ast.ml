(** Abstract syntax for quantified formulas *)

(** Defines the abstract syntax used by the {!module:Quant_parser} module *)

(* Copyright (C) 2019 The MITRE Corporation

   This program is free software: you can redistribute it and/or
   modify it under the terms of the BSD License as published by the
   University of California. *)

type pos = Lexing.position

type ast_symbol = Sym of pos * string

type ast_term = Term of ast_symbol * ast_term list

type ast_atom =
  | Atom of ast_symbol * ast_term list
  | Equal of ast_term * ast_term

type ast_conj = Conj of ast_atom list

type ast_quant = Quant of ast_symbol list * ast_conj

type ast_disj = Disj of ast_quant list

type ast_form = Form of pos * ast_quant * ast_disj
