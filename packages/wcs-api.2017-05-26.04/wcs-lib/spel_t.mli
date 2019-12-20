(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

(** Spel abstract syntax tree. *)

type location = Lexing.position * Lexing.position

(** {6 Spel AST} *)

(** atomic types *)
type spel_type =
  | T_string
  | T_int
  | T_real
  | T_boolean
  | T_object

(** literals *)
type literal =
  | L_string of string
  | L_int of int
  | L_real of float
  | L_boolean of bool
  | L_null

(** operators *)
type op =
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_not
  | Op_and
  | Op_or
  | Op_plus
  | Op_minus
  | Op_uminus
  | Op_mult
  | Op_div
  | Op_mod
  | Op_concat
  | Op_toString

(** expressions *)
type expression =
  { expr_desc : expression_desc;
    expr_loc : location;
    mutable expr_text : string option }
and expression_desc =
  (* Spel expressions *)
  | E_lit of literal
  | E_prop of expression * string (** [e.x] *)
  | E_prop_catch of expression * string (** [e?.x] *)
  | E_get of expression * expression (** [e1[e2]] *)
  | E_list of expression list (** [{ e1, e2 .. }] *)
  | E_new_array of spel_type * int option list * expression list option (** [new T[]{ e1, e2 ... }] *)
  | E_new of string * expression list (** [new T(e1,e2...)] *)
  | E_call of expression option * string * expression list (** [e.m(e1,e2...)] *)
  | E_call_catch of expression option * string * expression list (** [e?.m(e1,e2...)] *)
  | E_op of op * expression list
  | E_conditional of expression * expression * expression (** [e1?e2:e3] *)
  | E_ident of string (** [v] *)
  (* WCS extensions *)
  | E_anything_else (** [anything_else] *)
  | E_context (** [context] *)
  | E_conversation_start (** [conversation_start] *)
  | E_entities (** [entities] *)
  | E_input (** [input] *)
  | E_intents (** [entities] *)
  | E_output (** [output] *)
  | E_variable of (string * string option) (** [$v] or [$v:(w)] *)
  | E_intent of string (** [#intent] *)
  | E_entity of (string * string option) (** [@a] or [@a:(b)] *)
  (* Fallback *)
  | E_error of string


type spel = expression

