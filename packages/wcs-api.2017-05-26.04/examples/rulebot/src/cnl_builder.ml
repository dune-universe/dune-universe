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

(** {6. Expression Builders} *)

let mk_expr_with_loc node ofname loc =
  { expr_node = node;
    expr_field = ofname;
    expr_loc = loc; }

let mk_expr node =
  mk_expr_with_loc node None (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_expr_undefined () =
  mk_expr (N_undefined None)

let mk_expr_f desc = mk_expr (N_filled (None, desc))

let mk_expr_in_field node field_info =
  mk_expr_with_loc node (Some field_info) (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_expr_in_field_undefined field_info =
  mk_expr_in_field (N_undefined None) field_info

let mk_expr_in_field_f desc field_info = mk_expr_in_field (N_filled (None, desc)) field_info

(** {6. Literal Builders} *)

let mk_boolean_f b =
  mk_expr_f (E_lit (L_boolean b))

let mk_int_f i =
  mk_expr_f (E_lit (L_int i))

let mk_int_as_string_f i =
  mk_expr_f (E_lit (L_int_as_string i))

let mk_float_f f =
  mk_expr_f (E_lit (L_real f))

let mk_float_as_string_f i =
  mk_expr_f (E_lit (L_real_as_string i))

let mk_boolean_as_string_f i =
  mk_expr_f (E_lit (L_boolean_as_string i))

let mk_string_f s =
  mk_expr_f (E_lit (L_string s))

let mk_enum_f s =
  mk_expr_f (E_lit (L_enum s))

let mk_var_f v =
  mk_expr_f (E_var v)

let mk_get_f e (fname:field_name) =
  mk_expr_f (E_get (e, fname))

let mk_aggregate_f e (op:Cnl_t.cnl_aggop) (fname:field_name) =
  mk_expr_f (E_agg (op, e, fname))

let mk_get_var_f (vname:variable_name) (fname:field_name) =
  mk_get_f (mk_var_f vname) fname


let mk_concat_f e1 e2 =
  mk_expr_f (E_binop (Op_concat, e1, e2))

let mk_concat_list_f el =
  begin match el with
  | [] -> mk_string_f ""
  | [e1] -> e1
  | e1::el -> List.fold_left mk_concat_f e1 el
  end

let mk_lt_f e1 e2 =
  mk_expr_f (E_binop (Op_lt, e1, e2))

let mk_plus_f e1 e2 =
  mk_expr_f (E_binop (Op_plus, e1, e2))

let mk_div_f e1 e2 =
  mk_expr_f (E_binop (Op_div, e1, e2))

let mk_binop_expr_f op e1 e2 =
  mk_expr_f (E_binop (op, e1, e2))

let mk_binop_f op =
  mk_expr_f (E_binop (op, mk_expr_undefined (), mk_expr_undefined ()))

let mk_unop_f op =
  mk_expr_f (E_unop (op, mk_expr_undefined ()))

let mk_this_f ename =
  mk_expr_f (E_this ename)

let mk_new_event_f ename setters =
  mk_expr_f (E_new (ename,setters))

let mk_new_event_for_concept_f ename field_names =
  let setters = List.map (fun fname -> (fname,mk_expr_in_field_undefined (ename,fname))) field_names in
  mk_new_event_f ename setters

let mk_avg_f fname e =
  mk_expr_f (E_agg (A_avg,e,fname))


(** {6. Event Builders} *)

let mk_evnt_with_loc node loc =
  { evnt_node = node;
    evnt_loc = loc; }

let mk_evnt node =
  mk_evnt_with_loc node (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_evnt_undefined () =
  mk_evnt (N_undefined None)

let mk_evnt_f desc = mk_evnt (N_filled (None, desc))


(** {6. Condition Builders} *)

let mk_cond_with_loc node loc =
  { cond_node = node;
    cond_loc = loc; }

let mk_cond node =
  mk_cond_with_loc node (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_cond_undefined () =
  mk_cond (N_undefined None)

let mk_cond_f desc = mk_cond (N_filled (None, desc))

(** {6. Action Builders} *)

let mk_actn_with_loc node loc =
  { actn_node = node;
    actn_loc = loc; }

let mk_actn node =
  mk_actn_with_loc node (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_actn_f node =
  mk_actn (N_filled (None, node))

let mk_actn_undefined () =
  mk_actn (N_undefined None)

let mk_print_f e =
  mk_actn_f (A_print e)

let mk_emit_f e =
  mk_actn_f (A_emit e)

let mk_define_f vname e =
  mk_actn_f (A_define (vname,e))

let mk_set_desc_f fname vname e =
  A_set (fname,vname,e)

let mk_set_f fname vname e =
  mk_actn_f (mk_set_desc_f fname vname e)

(** {6. Actions Builders} *)

let mk_actns_with_loc node loc =
  { actns_node = node;
    actns_loc = loc; }

let mk_actns node =
  mk_actns_with_loc node (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ())

let mk_actns_undefined () =
  mk_actns (N_undefined None)

let mk_actns_f desc =
  mk_actns (N_filled (None, desc))

let mk_actns_desc_undefined () : cnl_actns_desc =
  { list_elems = [ mk_actn_undefined () ];
    list_closed = N_undefined None; }

(** {6. Rule Builders} *)

let mk_rule_desc evnt cond actns =
  { rule_evnt = evnt;
    rule_cond = cond;
    rule_actns = actns; }

let mk_rule_f evnt cond actns =
  { rule_node = N_filled (None, mk_rule_desc evnt cond actns);
    rule_loc = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); }

let mk_rule_init evnt_desc cond_desc actns_desc =
  mk_rule_f
    (mk_evnt_f evnt_desc)
    (mk_cond_f cond_desc)
    (mk_actns_f actns_desc)

let mk_rule_undefined () =
  { rule_node = N_undefined None;
    rule_loc = (Parsing.symbol_start_pos (), Parsing.symbol_end_pos ()); }
