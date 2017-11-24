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

open Cnl_t
open Cnl_util

open Format

let cnl_print_literal prec ff l =
  begin match l with
  | L_string s -> fprintf ff "\"%s\"" s
  | L_int i -> fprintf ff "%i" i
  | L_int_as_string i -> fprintf ff "%s" i
  | L_real f -> fprintf ff "%f" f
  | L_real_as_string f -> fprintf ff "%s" f
  | L_boolean true -> fprintf ff "true"
  | L_boolean false -> fprintf ff "false"
  | L_boolean_as_string b -> fprintf ff "%s" b
  | L_enum s -> fprintf ff "%s" s
  | L_date s -> fprintf ff "%s" s
  | L_duration s -> fprintf ff "%s" s
  end


let cnl_binop_precedence op =
  begin match op with
  | Op_eq -> 3
  | Op_ne -> 3
  | Op_lt -> 3
  | Op_le -> 3
  | Op_gt -> 3
  | Op_ge -> 3
  | Op_and -> 2
  | Op_or -> 1
  | Op_plus -> 4
  | Op_minus -> 4
  | Op_mult -> 5
  | Op_div -> 5
  | Op_mod -> 5
  | Op_pow -> 5
  | Op_concat -> 5
  | Op_during -> 4
  end


let variable_has_whitespace (n:string) : bool =
  String.contains n ' ' || String.contains n '\t'

let cnl_print_variable ff n =
  if (variable_has_whitespace n)
  then fprintf ff "'%s'" n
  else fprintf ff "%s" n

let cnl_print_kind kind =
  begin match kind with
  | K_expr _ -> "expr"
  | K_actn -> "action"
  | K_evnt -> "event"
  | K_cond -> "condition"
  | K_actns -> "actions"
  | K_actns_closed -> "actions_closed"
  | K_rule -> "rule"
  end

let cnl_print_id kind ff (i: id) =
  begin match i with
  | None -> ()
  | Some i -> fprintf ff "{%s%i}" (cnl_print_kind kind) i
  end

let cnl_print_node fpart kind ff part =
  begin match part with
  | N_undefined i ->
      fprintf ff "[UNDEFINED %a]" (cnl_print_id kind) i
  | N_filled (i, x) ->
      fprintf ff "[%a%a]" (cnl_print_id kind) i fpart x
  | N_rejected (i, x) ->
      fprintf ff "[REJECTED %a%a]" (cnl_print_id kind) i fpart x
  | N_accepted x ->
      fprintf ff "%a" fpart x
  end

let rec cnl_print_expr prec ff e =
  cnl_print_expr_desc prec ff e.expr_node
and cnl_print_expr_desc prec ff ed =
  let printer ff x =
    begin match x with
    | E_lit l ->
        cnl_print_literal prec ff l
    | E_var n ->
        cnl_print_variable ff n
    | E_get (e,fname) ->
        fprintf ff "the %s of %a"
          fname
          (cnl_print_expr prec) e
    | E_agg (agg, e, fname) ->
        fprintf ff "the %s %s of all %a"
          (string_of_cnl_agg agg)
          fname
          (cnl_print_expr prec) e
    | E_unop (op,e) ->
        fprintf ff "%s %a"
          (string_of_cnl_unop op)
          (cnl_print_expr prec) e
    | E_binop (op,e1,e2) ->
        let new_prec = cnl_binop_precedence op in
        if new_prec < prec then
          fprintf ff "(%a %s %a)"
            (cnl_print_expr new_prec) e1
            (string_of_cnl_binop op)
            (cnl_print_expr new_prec) e2
        else
          fprintf ff "%a %s %a"
            (cnl_print_expr new_prec) e1
            (string_of_cnl_binop op)
            (cnl_print_expr new_prec) e2
    | E_error j ->
        fprintf ff "[ERROR]"
    | E_this s ->
        fprintf ff "this %s event" s
    | E_new (c,sl) ->
        fprintf ff "@[<hv 2>a new %s where@ %a@]" c cnl_print_setters sl
    end
  in
  cnl_print_node printer (K_expr None) ff ed
and cnl_print_setter ff s =
  let (fname, e) = s in
  fprintf ff "the %s is %a" fname (cnl_print_expr 0) e
and cnl_print_setters ff sl =
  begin match sl with
  | [] -> ()
  | [s] -> cnl_print_setter ff s
  | s::sl' -> fprintf ff "%a,@ %a" cnl_print_setter s cnl_print_setters sl'
  end

let cnl_print_expr_top = cnl_print_expr 0

let cnl_print_evnt ff evnt =
  let printer ff w =
    let (ename,ovname) = w in
    begin match ovname with
    | None ->
        fprintf ff "a %s occurs" ename
    | Some vname ->
        fprintf ff "a %s occurs, called '%s'" ename vname
    end
  in
  fprintf ff "@[<hv 2>when %a@;<0 -2>@]"
    (cnl_print_node printer K_evnt) evnt.evnt_node

let cnl_print_cond ff cond =
  begin match cond.cond_node with
  | N_undefined _ ->
      let printer ff i = () in
      fprintf ff "@[<hv 2>if %a@;<0 -2>@]"
        (cnl_print_node printer K_cond) cond.cond_node
  | _ ->
      let printer ff i =
        begin match i with
        | C_condition i -> fprintf ff "@[<hv 2>if %a@;<0 -2>@]" (cnl_print_expr 0) i
        | C_no_condition -> ()
        end
      in
      cnl_print_node printer K_cond ff cond.cond_node
  end

let cnl_print_actn ff a =
  let printer ff ad =
    begin match ad with
    | A_print e ->
        fprintf ff "print %a" (cnl_print_expr 0) e
    | A_emit e ->
        fprintf ff "emit %a" (cnl_print_expr 0) e
    | A_define (vname,e) ->
        fprintf ff "define '%s' as@ %a" vname (cnl_print_expr 0) e
    | A_set (fname,vname,e) ->
        fprintf ff "set the %s of '%s' to %a" fname vname (cnl_print_expr 0) e
    end
  in
  cnl_print_node printer K_actn ff a.actn_node

let rec cnl_print_actns_desc ff al =
  begin match al with
  | [] -> ()
  | [a] -> fprintf ff "%a;" cnl_print_actn a
  | a::al' -> fprintf ff "%a;@ %a" cnl_print_actn a cnl_print_actns_desc al'
  end

let cnl_print_actns ff actns =
  let printer ff t =
    fprintf ff "%a" cnl_print_actns_desc t.list_elems
  in
  fprintf ff "@[<hv 2>then %a@;<0 -2>@]"
    (cnl_print_node printer K_actns) actns.actns_node

let cnl_print_rule_desc ff r =
  fprintf ff "%a@ %a@ %a"
    cnl_print_evnt r.rule_evnt
    cnl_print_cond r.rule_cond
    cnl_print_actns r.rule_actns

let cnl_print_rule ff r =
  cnl_print_node cnl_print_rule_desc K_rule ff r.rule_node

let cnl_print_rule_top r =
  let ff = str_formatter in
  begin
    fprintf ff "@[%a@]@." cnl_print_rule r;
    flush_str_formatter ()
  end
