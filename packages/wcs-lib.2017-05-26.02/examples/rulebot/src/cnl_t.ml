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

open Json_t
open Deriving_intf

let (>>=) x f =
  match x with Ok x -> f x | (Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> Ok (List.rev acc)

type 'a error_or = ('a, string) result

[@@@ocaml.text " {6. Locations} "]
type location = (Lexing.position* Lexing.position)
let location_to_yojson pos = `Null
let location_of_yojson j =
  Ok
    ({
      Lexing.pos_fname = "";
      Lexing.pos_lnum = 0;
      Lexing.pos_bol = 0;
      Lexing.pos_cnum = 0
    },
      {
        Lexing.pos_fname = "";
        Lexing.pos_lnum = 0;
        Lexing.pos_bol = 0;
        Lexing.pos_cnum = 0
      })
let default_loc =
  ((Parsing.symbol_start_pos ()), (Parsing.symbol_end_pos ()))
[@@@ocaml.text " {6. Nodes}"]
type id = int option
let rec (id_to_yojson : id -> Yojson.Safe.json) =
  ((
    function | None  -> `Null | Some x -> ((fun x  -> `Int x)) x)[@ocaml.warning
      "-A"])
and (id_of_yojson :
       Yojson.Safe.json -> id error_or)
  =
  ((
    function
    | `Null -> Ok None
    | x ->
        ((function | `Int x -> Ok x | _ -> Error "Cnl_t.id")
           x)
        >>= ((fun x  -> Ok (Some x))))[@ocaml.warning "-A"])
type 'a node =
  | N_undefined of id
  | N_filled of id* 'a
  | N_rejected of id* 'a
  | N_accepted of 'a
let rec node_to_yojson :
  'a . ('a -> Yojson.Safe.json) -> 'a node -> Yojson.Safe.json=
  fun poly_a  ->
    ((
      function
      | N_undefined arg0 ->
          `List [`String "N_undefined"; ((fun x  -> id_to_yojson x)) arg0]
      | N_filled (arg0,arg1) ->
          `List
            [`String "N_filled";
             ((fun x  -> id_to_yojson x)) arg0;
             (poly_a : _ -> Yojson.Safe.json) arg1]
      | N_rejected (arg0,arg1) ->
          `List
            [`String "N_rejected";
             ((fun x  -> id_to_yojson x)) arg0;
             (poly_a : _ -> Yojson.Safe.json) arg1]
      | N_accepted arg0 ->
          `List
            [`String "N_accepted"; (poly_a : _ -> Yojson.Safe.json) arg0])
        [@ocaml.warning "-A"])
and node_of_yojson :
  'a .
    (Yojson.Safe.json -> 'a error_or) ->
  Yojson.Safe.json -> 'a node error_or=
  fun poly_a  ->
    ((
      function
      | `List ((`String "N_undefined")::arg0::[]) ->
          ((fun x  -> id_of_yojson x) arg0) >>=
          ((fun arg0  -> Ok (N_undefined arg0)))
      | `List ((`String "N_filled")::arg0::arg1::[]) ->
          ((poly_a : Yojson.Safe.json -> _ error_or) arg1) >>=
          ((fun arg1  ->
             ((fun x  -> id_of_yojson x) arg0) >>=
             (fun arg0  -> Ok (N_filled (arg0, arg1)))))
      | `List ((`String "N_rejected")::arg0::arg1::[]) ->
          ((poly_a : Yojson.Safe.json -> _ error_or) arg1) >>=
          ((fun arg1  ->
             ((fun x  -> id_of_yojson x) arg0) >>=
             (fun arg0  -> Ok (N_rejected (arg0, arg1)))))
      | `List ((`String "N_accepted")::arg0::[]) ->
          ((poly_a : Yojson.Safe.json -> _ error_or) arg0) >>=
          ((fun arg0  -> Ok (N_accepted arg0)))
      | _ -> Error "Cnl_t.node")[@ocaml.warning "-A"])
type 'a node_list = {
  list_elems: 'a list;
  list_closed: unit node;}
let rec node_list_to_yojson :
  'a . ('a -> Yojson.Safe.json) -> 'a node_list -> Yojson.Safe.json=
  fun poly_a  ->
    ((
      fun x  ->
        let fields = [] in
        let fields =
          ("list_closed",
           ((fun x  -> (node_to_yojson (fun x  -> `Null)) x) x.list_closed))
          :: fields in
        let fields =
          ("list_elems",
           ((fun x  -> `List (List.map (poly_a : _ -> Yojson.Safe.json) x))
              x.list_elems))
          :: fields in
        `Assoc fields)[@ocaml.warning "-A"])
and node_list_of_yojson :
  'a .
    (Yojson.Safe.json -> 'a error_or) ->
  Yojson.Safe.json -> 'a node_list error_or=
  fun poly_a  ->
    ((
      function
      | `Assoc xs ->
          let rec loop xs ((arg0,arg1) as _state) =
            match xs with
            | ("list_elems",x)::xs ->
                loop xs
                  (((function
                   | `List xs ->
                       map_bind (poly_a : Yojson.Safe.json -> _ error_or)
                         [] xs
                   | _ -> Error "Cnl_t.node_list.list_elems") x),
                   arg1)
            | ("list_closed",x)::xs ->
                loop xs
                  (arg0,
                   ((fun x  ->
                      (node_of_yojson
                         (function
                         | `Null -> Ok ()
                         | _ ->
                             Error "Cnl_t.node_list.list_closed"))
                        x) x))
            | [] ->
                arg1 >>=
                ((fun arg1  ->
                   arg0 >>=
                   (fun arg0  ->
                      Ok
                        { list_elems = arg0; list_closed = arg1 })))
            | _::xs -> Error "Cnl_t.node_list" in
          loop xs
            ((Error "Cnl_t.node_list.list_elems"),
             (Error "Cnl_t.node_list.list_closed"))
      | _ -> Error "Cnl_t.node_list")[@ocaml.warning "-A"])
[@@@ocaml.text
  " {6. AST}\n    See BNF in ../papers/2017-debs-dialog-odm/cnl_bnf.txt\n "]
type cnl_rule =
  {
    rule_node: cnl_rule_desc node;
    rule_loc: ((location)[@default default_loc]);}
and cnl_rule_desc =
  {
    rule_evnt: cnl_event;
    rule_cond: cnl_cond;
    rule_actns: cnl_actions;}
and cnl_event =
  {
    evnt_node: cnl_evnt_desc node;
    evnt_loc: ((location)[@default default_loc]);}
and cnl_evnt_desc = (event_name* variable_name option)
and cnl_cond =
  {
    cond_node: cnl_cond_desc node;
    cond_loc: ((location)[@default default_loc]);}
and cnl_cond_desc =
  | C_no_condition
  | C_condition of cnl_expr
and cnl_actions =
  {
    actns_node: cnl_actns_desc node;
    actns_loc: ((location)[@default default_loc]);}
and cnl_actns_desc = cnl_action node_list
and cnl_action =
  {
    actn_node: cnl_actn_desc node;
    actn_loc: ((location)[@default default_loc]);}
and cnl_actn_desc =
  | A_print of cnl_expr
  | A_emit of cnl_expr
  | A_define of variable_name* cnl_expr
  | A_set of field_name* variable_name* cnl_expr
and cnl_expr =
  {
    expr_node: cnl_expr_desc node;
    expr_field: (event_name* field_name) option;
    expr_loc: ((location)[@default default_loc]);}
and cnl_expr_desc =
  | E_lit of cnl_literal
  | E_var of variable_name
  | E_get of cnl_expr* field_name
  | E_agg of cnl_aggop* cnl_expr* field_name
  | E_unop of cnl_unop* cnl_expr
  | E_binop of cnl_binop* cnl_expr* cnl_expr
  | E_error of Yojson.Safe.json
  | E_this of event_name
  | E_new of event_name* cnl_setter list
and cnl_setter = (field_name* cnl_expr)
and cnl_literal =
  | L_string of string
  | L_int of int
  | L_int_as_string of string
  | L_real of float
  | L_real_as_string of string
  | L_boolean of bool
  | L_boolean_as_string of string
  | L_enum of string
  | L_date of string
  | L_duration of string
and event_name = string
and variable_name = string
and field_name = string
and cnl_unop =
  | Op_not
  | Op_toString
and cnl_binop =
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_and
  | Op_or
  | Op_plus
  | Op_minus
  | Op_mult
  | Op_div
  | Op_mod
  | Op_pow
  | Op_concat
  | Op_during
and cnl_aggop =
  | A_total
  | A_avg
let rec (cnl_rule_to_yojson : cnl_rule -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        if x.rule_loc = default_loc
        then fields
        else ("rule_loc", (((fun x  -> location_to_yojson x)) x.rule_loc))
             :: fields in
      let fields =
        ("rule_node",
         ((fun x  ->
            (node_to_yojson (fun x  -> cnl_rule_desc_to_yojson x)) x)
            x.rule_node))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_rule_of_yojson :
       Yojson.Safe.json -> cnl_rule error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1) as _state) =
          match xs with
          | ("rule_node",x)::xs ->
              loop xs
                (((fun x  ->
                   (node_of_yojson (fun x  -> cnl_rule_desc_of_yojson x))
                     x) x), arg1)
          | ("rule_loc",x)::xs ->
              loop xs (arg0, ((fun x  -> location_of_yojson x) x))
          | [] ->
              arg1 >>=
              ((fun arg1  ->
                 arg0 >>=
                 (fun arg0  ->
                    Ok { rule_node = arg0; rule_loc = arg1 })))
          | _::xs -> Error "Cnl_t.cnl_rule" in
        loop xs
          ((Error "Cnl_t.cnl_rule.rule_node"),
           (Ok default_loc))
    | _ -> Error "Cnl_t.cnl_rule")[@ocaml.warning "-A"])
and (cnl_rule_desc_to_yojson : cnl_rule_desc -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        ("rule_actns", ((fun x  -> cnl_actions_to_yojson x) x.rule_actns))
        :: fields in
      let fields =
        ("rule_cond", ((fun x  -> cnl_cond_to_yojson x) x.rule_cond)) ::
        fields in
      let fields =
        ("rule_evnt", ((fun x  -> cnl_event_to_yojson x) x.rule_evnt)) ::
        fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_rule_desc_of_yojson :
       Yojson.Safe.json -> cnl_rule_desc error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1,arg2) as _state) =
          match xs with
          | ("rule_evnt",x)::xs ->
              loop xs (((fun x  -> cnl_event_of_yojson x) x), arg1, arg2)
          | ("rule_cond",x)::xs ->
              loop xs (arg0, ((fun x  -> cnl_cond_of_yojson x) x), arg2)
          | ("rule_actns",x)::xs ->
              loop xs (arg0, arg1, ((fun x  -> cnl_actions_of_yojson x) x))
          | [] ->
              arg2 >>=
              ((fun arg2  ->
                 arg1 >>=
                 (fun arg1  ->
                    arg0 >>=
                    (fun arg0  ->
                       Ok
                         {
                           rule_evnt = arg0;
                           rule_cond = arg1;
                           rule_actns = arg2
                         }))))
          | _::xs -> Error "Cnl_t.cnl_rule_desc" in
        loop xs
          ((Error "Cnl_t.cnl_rule_desc.rule_evnt"),
           (Error "Cnl_t.cnl_rule_desc.rule_cond"),
           (Error "Cnl_t.cnl_rule_desc.rule_actns"))
    | _ -> Error "Cnl_t.cnl_rule_desc")[@ocaml.warning "-A"])
and (cnl_event_to_yojson : cnl_event -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        if x.evnt_loc = default_loc
        then fields
        else ("evnt_loc", (((fun x  -> location_to_yojson x)) x.evnt_loc))
             :: fields in
      let fields =
        ("evnt_node",
         ((fun x  ->
            (node_to_yojson (fun x  -> cnl_evnt_desc_to_yojson x)) x)
            x.evnt_node))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_event_of_yojson :
       Yojson.Safe.json -> cnl_event error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1) as _state) =
          match xs with
          | ("evnt_node",x)::xs ->
              loop xs
                (((fun x  ->
                   (node_of_yojson (fun x  -> cnl_evnt_desc_of_yojson x))
                     x) x), arg1)
          | ("evnt_loc",x)::xs ->
              loop xs (arg0, ((fun x  -> location_of_yojson x) x))
          | [] ->
              arg1 >>=
              ((fun arg1  ->
                 arg0 >>=
                 (fun arg0  ->
                    Ok { evnt_node = arg0; evnt_loc = arg1 })))
          | _::xs -> Error "Cnl_t.cnl_event" in
        loop xs
          ((Error "Cnl_t.cnl_event.evnt_node"),
           (Ok default_loc))
    | _ -> Error "Cnl_t.cnl_event")[@ocaml.warning "-A"])
and (cnl_evnt_desc_to_yojson : cnl_evnt_desc -> Yojson.Safe.json) =
  ((
    fun (arg0,arg1)  ->
      `List
        [((fun x  -> event_name_to_yojson x)) arg0;
         ((function
          | None  -> `Null
          | Some x -> ((fun x  -> variable_name_to_yojson x)) x)) arg1])
      [@ocaml.warning "-A"])
and (cnl_evnt_desc_of_yojson :
       Yojson.Safe.json -> cnl_evnt_desc error_or)
  =
  ((
    function
    | `List (arg0::arg1::[]) ->
        ((function
         | `Null -> Ok None
         | x ->
             ((fun x  -> variable_name_of_yojson x) x) >>=
             ((fun x  -> Ok (Some x)))) arg1)
        >>=
        ((fun arg1  ->
           ((fun x  -> event_name_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (arg0, arg1))))
    | _ -> Error "Cnl_t.cnl_evnt_desc")[@ocaml.warning "-A"])
and (cnl_cond_to_yojson : cnl_cond -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        if x.cond_loc = default_loc
        then fields
        else ("cond_loc", (((fun x  -> location_to_yojson x)) x.cond_loc))
             :: fields in
      let fields =
        ("cond_node",
         ((fun x  ->
            (node_to_yojson (fun x  -> cnl_cond_desc_to_yojson x)) x)
            x.cond_node))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_cond_of_yojson :
       Yojson.Safe.json -> cnl_cond error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1) as _state) =
          match xs with
          | ("cond_node",x)::xs ->
              loop xs
                (((fun x  ->
                   (node_of_yojson (fun x  -> cnl_cond_desc_of_yojson x))
                     x) x), arg1)
          | ("cond_loc",x)::xs ->
              loop xs (arg0, ((fun x  -> location_of_yojson x) x))
          | [] ->
              arg1 >>=
              ((fun arg1  ->
                 arg0 >>=
                 (fun arg0  ->
                    Ok { cond_node = arg0; cond_loc = arg1 })))
          | _::xs -> Error "Cnl_t.cnl_cond" in
        loop xs
          ((Error "Cnl_t.cnl_cond.cond_node"),
           (Ok default_loc))
    | _ -> Error "Cnl_t.cnl_cond")[@ocaml.warning "-A"])
and (cnl_cond_desc_to_yojson : cnl_cond_desc -> Yojson.Safe.json) =
  ((
    function
    | C_no_condition  -> `List [`String "C_no_condition"]
    | C_condition arg0 ->
        `List
          [`String "C_condition"; ((fun x  -> cnl_expr_to_yojson x)) arg0])
      [@ocaml.warning "-A"])
and (cnl_cond_desc_of_yojson :
       Yojson.Safe.json -> cnl_cond_desc error_or)
  =
  ((
    function
    | `List ((`String "C_no_condition")::[]) -> Ok C_no_condition
    | `List ((`String "C_condition")::arg0::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (C_condition arg0)))
    | _ -> Error "Cnl_t.cnl_cond_desc")[@ocaml.warning "-A"])
and (cnl_actions_to_yojson : cnl_actions -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        if x.actns_loc = default_loc
        then fields
        else
          ("actns_loc", (((fun x  -> location_to_yojson x)) x.actns_loc))
          :: fields in
      let fields =
        ("actns_node",
         ((fun x  ->
            (node_to_yojson (fun x  -> cnl_actns_desc_to_yojson x)) x)
            x.actns_node))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_actions_of_yojson :
       Yojson.Safe.json -> cnl_actions error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1) as _state) =
          match xs with
          | ("actns_node",x)::xs ->
              loop xs
                (((fun x  ->
                   (node_of_yojson (fun x  -> cnl_actns_desc_of_yojson x))
                     x) x), arg1)
          | ("actns_loc",x)::xs ->
              loop xs (arg0, ((fun x  -> location_of_yojson x) x))
          | [] ->
              arg1 >>=
              ((fun arg1  ->
                 arg0 >>=
                 (fun arg0  ->
                    Ok { actns_node = arg0; actns_loc = arg1 })))
          | _::xs -> Error "Cnl_t.cnl_actions" in
        loop xs
          ((Error "Cnl_t.cnl_actions.actns_node"),
           (Ok default_loc))
    | _ -> Error "Cnl_t.cnl_actions")[@ocaml.warning "-A"])
and (cnl_actns_desc_to_yojson : cnl_actns_desc -> Yojson.Safe.json) =
  ((
    fun x  -> (node_list_to_yojson (fun x  -> cnl_action_to_yojson x)) x)
      [@ocaml.warning "-A"])
and (cnl_actns_desc_of_yojson :
       Yojson.Safe.json -> cnl_actns_desc error_or)
  =
  ((
    fun x  -> (node_list_of_yojson (fun x  -> cnl_action_of_yojson x)) x)
      [@ocaml.warning "-A"])
and (cnl_action_to_yojson : cnl_action -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        if x.actn_loc = default_loc
        then fields
        else ("actn_loc", (((fun x  -> location_to_yojson x)) x.actn_loc))
             :: fields in
      let fields =
        ("actn_node",
         ((fun x  ->
            (node_to_yojson (fun x  -> cnl_actn_desc_to_yojson x)) x)
            x.actn_node))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_action_of_yojson :
       Yojson.Safe.json -> cnl_action error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1) as _state) =
          match xs with
          | ("actn_node",x)::xs ->
              loop xs
                (((fun x  ->
                   (node_of_yojson (fun x  -> cnl_actn_desc_of_yojson x))
                     x) x), arg1)
          | ("actn_loc",x)::xs ->
              loop xs (arg0, ((fun x  -> location_of_yojson x) x))
          | [] ->
              arg1 >>=
              ((fun arg1  ->
                 arg0 >>=
                 (fun arg0  ->
                    Ok { actn_node = arg0; actn_loc = arg1 })))
          | _::xs -> Error "Cnl_t.cnl_action" in
        loop xs
          ((Error "Cnl_t.cnl_action.actn_node"),
           (Ok default_loc))
    | _ -> Error "Cnl_t.cnl_action")[@ocaml.warning "-A"])
and (cnl_actn_desc_to_yojson : cnl_actn_desc -> Yojson.Safe.json) =
  ((
    function
    | A_print arg0 ->
        `List [`String "A_print"; ((fun x  -> cnl_expr_to_yojson x)) arg0]
    | A_emit arg0 ->
        `List [`String "A_emit"; ((fun x  -> cnl_expr_to_yojson x)) arg0]
    | A_define (arg0,arg1) ->
        `List
          [`String "A_define";
           ((fun x  -> variable_name_to_yojson x)) arg0;
           ((fun x  -> cnl_expr_to_yojson x)) arg1]
    | A_set (arg0,arg1,arg2) ->
        `List
          [`String "A_set";
           ((fun x  -> field_name_to_yojson x)) arg0;
           ((fun x  -> variable_name_to_yojson x)) arg1;
           ((fun x  -> cnl_expr_to_yojson x)) arg2])[@ocaml.warning "-A"])
and (cnl_actn_desc_of_yojson :
       Yojson.Safe.json -> cnl_actn_desc error_or)
  =
  ((
    function
    | `List ((`String "A_print")::arg0::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (A_print arg0)))
    | `List ((`String "A_emit")::arg0::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (A_emit arg0)))
    | `List ((`String "A_define")::arg0::arg1::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((fun x  -> variable_name_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (A_define (arg0, arg1)))))
    | `List ((`String "A_set")::arg0::arg1::arg2::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg2) >>=
        ((fun arg2  ->
           ((fun x  -> variable_name_of_yojson x) arg1) >>=
           (fun arg1  ->
              ((fun x  -> field_name_of_yojson x) arg0) >>=
              (fun arg0  -> Ok (A_set (arg0, arg1, arg2))))))
    | _ -> Error "Cnl_t.cnl_actn_desc")[@ocaml.warning "-A"])
and (cnl_expr_to_yojson : cnl_expr -> Yojson.Safe.json) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        if x.expr_loc = default_loc
        then fields
        else ("expr_loc", (((fun x  -> location_to_yojson x)) x.expr_loc))
             :: fields in
      let fields =
        ("expr_field",
         ((function
          | None  -> `Null
          | Some x ->
              ((fun (arg0,arg1)  ->
                 `List
                   [((fun x  -> event_name_to_yojson x)) arg0;
                    ((fun x  -> field_name_to_yojson x)) arg1])) x)
            x.expr_field))
        :: fields in
      let fields =
        ("expr_node",
         ((fun x  ->
            (node_to_yojson (fun x  -> cnl_expr_desc_to_yojson x)) x)
            x.expr_node))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (cnl_expr_of_yojson :
       Yojson.Safe.json -> cnl_expr error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1,arg2) as _state) =
          match xs with
          | ("expr_node",x)::xs ->
              loop xs
                (((fun x  ->
                   (node_of_yojson (fun x  -> cnl_expr_desc_of_yojson x))
                     x) x), arg1, arg2)
          | ("expr_field",x)::xs ->
              loop xs
                (arg0,
                 ((function
                  | `Null -> Ok None
                  | x ->
                      ((function
                       | `List (arg0::arg1::[]) ->
                           ((fun x  -> field_name_of_yojson x) arg1) >>=
                           ((fun arg1  ->
                              ((fun x  -> event_name_of_yojson x)
                                 arg0)
                              >>=
                              (fun arg0  -> Ok (arg0, arg1))))
                       | _ -> Error "Cnl_t.cnl_expr.expr_field")
                         x)
                      >>= ((fun x  -> Ok (Some x)))) x), arg2)
          | ("expr_loc",x)::xs ->
              loop xs (arg0, arg1, ((fun x  -> location_of_yojson x) x))
          | [] ->
              arg2 >>=
              ((fun arg2  ->
                 arg1 >>=
                 (fun arg1  ->
                    arg0 >>=
                    (fun arg0  ->
                       Ok
                         {
                           expr_node = arg0;
                           expr_field = arg1;
                           expr_loc = arg2
                         }))))
          | _::xs -> Error "Cnl_t.cnl_expr" in
        loop xs
          ((Error "Cnl_t.cnl_expr.expr_node"),
           (Error "Cnl_t.cnl_expr.expr_field"),
           (Ok default_loc))
    | _ -> Error "Cnl_t.cnl_expr")[@ocaml.warning "-A"])
and (cnl_expr_desc_to_yojson : cnl_expr_desc -> Yojson.Safe.json) =
  ((
    function
    | E_lit arg0 ->
        `List [`String "E_lit"; ((fun x  -> cnl_literal_to_yojson x)) arg0]
    | E_var arg0 ->
        `List
          [`String "E_var"; ((fun x  -> variable_name_to_yojson x)) arg0]
    | E_get (arg0,arg1) ->
        `List
          [`String "E_get";
           ((fun x  -> cnl_expr_to_yojson x)) arg0;
           ((fun x  -> field_name_to_yojson x)) arg1]
    | E_agg (arg0,arg1,arg2) ->
        `List
          [`String "E_agg";
           ((fun x  -> cnl_aggop_to_yojson x)) arg0;
           ((fun x  -> cnl_expr_to_yojson x)) arg1;
           ((fun x  -> field_name_to_yojson x)) arg2]
    | E_unop (arg0,arg1) ->
        `List
          [`String "E_unop";
           ((fun x  -> cnl_unop_to_yojson x)) arg0;
           ((fun x  -> cnl_expr_to_yojson x)) arg1]
    | E_binop (arg0,arg1,arg2) ->
        `List
          [`String "E_binop";
           ((fun x  -> cnl_binop_to_yojson x)) arg0;
           ((fun x  -> cnl_expr_to_yojson x)) arg1;
           ((fun x  -> cnl_expr_to_yojson x)) arg2]
    | E_error arg0 -> `List [`String "E_error"; ((fun x  -> x)) arg0]
    | E_this arg0 ->
        `List [`String "E_this"; ((fun x  -> event_name_to_yojson x)) arg0]
    | E_new (arg0,arg1) ->
        `List
          [`String "E_new";
           ((fun x  -> event_name_to_yojson x)) arg0;
           ((fun x  -> `List (List.map (fun x  -> cnl_setter_to_yojson x) x)))
             arg1])[@ocaml.warning "-A"])
and (cnl_expr_desc_of_yojson :
       Yojson.Safe.json -> cnl_expr_desc error_or)
  =
  ((
    function
    | `List ((`String "E_lit")::arg0::[]) ->
        ((fun x  -> cnl_literal_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (E_lit arg0)))
    | `List ((`String "E_var")::arg0::[]) ->
        ((fun x  -> variable_name_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (E_var arg0)))
    | `List ((`String "E_get")::arg0::arg1::[]) ->
        ((fun x  -> field_name_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((fun x  -> cnl_expr_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_get (arg0, arg1)))))
    | `List ((`String "E_agg")::arg0::arg1::arg2::[]) ->
        ((fun x  -> field_name_of_yojson x) arg2) >>=
        ((fun arg2  ->
           ((fun x  -> cnl_expr_of_yojson x) arg1) >>=
           (fun arg1  ->
              ((fun x  -> cnl_aggop_of_yojson x) arg0) >>=
              (fun arg0  -> Ok (E_agg (arg0, arg1, arg2))))))
    | `List ((`String "E_unop")::arg0::arg1::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((fun x  -> cnl_unop_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_unop (arg0, arg1)))))
    | `List ((`String "E_binop")::arg0::arg1::arg2::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg2) >>=
        ((fun arg2  ->
           ((fun x  -> cnl_expr_of_yojson x) arg1) >>=
           (fun arg1  ->
              ((fun x  -> cnl_binop_of_yojson x) arg0) >>=
              (fun arg0  -> Ok (E_binop (arg0, arg1, arg2))))))
    | `List ((`String "E_error")::arg0::[]) ->
        ((fun x  -> Ok x) arg0) >>=
        ((fun arg0  -> Ok (E_error arg0)))
    | `List ((`String "E_this")::arg0::[]) ->
        ((fun x  -> event_name_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (E_this arg0)))
    | `List ((`String "E_new")::arg0::arg1::[]) ->
        ((function
         | `List xs -> map_bind (fun x  -> cnl_setter_of_yojson x) [] xs
         | _ -> Error "Cnl_t.cnl_expr_desc") arg1) >>=
        ((fun arg1  ->
           ((fun x  -> event_name_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_new (arg0, arg1)))))
    | _ -> Error "Cnl_t.cnl_expr_desc")[@ocaml.warning "-A"])
and (cnl_setter_to_yojson : cnl_setter -> Yojson.Safe.json) =
  ((
    fun (arg0,arg1)  ->
      `List
        [((fun x  -> field_name_to_yojson x)) arg0;
         ((fun x  -> cnl_expr_to_yojson x)) arg1])[@ocaml.warning "-A"])
and (cnl_setter_of_yojson :
       Yojson.Safe.json -> cnl_setter error_or)
  =
  ((
    function
    | `List (arg0::arg1::[]) ->
        ((fun x  -> cnl_expr_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((fun x  -> field_name_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (arg0, arg1))))
    | _ -> Error "Cnl_t.cnl_setter")[@ocaml.warning "-A"])
and (cnl_literal_to_yojson : cnl_literal -> Yojson.Safe.json) =
  ((
    function
    | L_string arg0 ->
        `List [`String "L_string"; ((fun x  -> `String x)) arg0]
    | L_int arg0 -> `List [`String "L_int"; ((fun x  -> `Int x)) arg0]
    | L_int_as_string arg0 ->
        `List [`String "L_int_as_string"; ((fun x  -> `String x)) arg0]
    | L_real arg0 -> `List [`String "L_real"; ((fun x  -> `Float x)) arg0]
    | L_real_as_string arg0 ->
        `List [`String "L_real_as_string"; ((fun x  -> `String x)) arg0]
    | L_boolean arg0 ->
        `List [`String "L_boolean"; ((fun x  -> `Bool x)) arg0]
    | L_boolean_as_string arg0 ->
        `List [`String "L_boolean_as_string"; ((fun x  -> `String x)) arg0]
    | L_enum arg0 -> `List [`String "L_enum"; ((fun x  -> `String x)) arg0]
    | L_date arg0 -> `List [`String "L_date"; ((fun x  -> `String x)) arg0]
    | L_duration arg0 ->
        `List [`String "L_duration"; ((fun x  -> `String x)) arg0])
      [@ocaml.warning "-A"])
and (cnl_literal_of_yojson :
       Yojson.Safe.json -> cnl_literal error_or)
  =
  ((
    function
    | `List ((`String "L_string")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_string arg0)))
    | `List ((`String "L_int")::arg0::[]) ->
        ((function
         | `Int x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_int arg0)))
    | `List ((`String "L_int_as_string")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_int_as_string arg0)))
    | `List ((`String "L_real")::arg0::[]) ->
        ((function
         | `Int x -> Ok (float_of_int x)
         | `Intlit x -> Ok (float_of_string x)
         | `Float x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_real arg0)))
    | `List ((`String "L_real_as_string")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_real_as_string arg0)))
    | `List ((`String "L_boolean")::arg0::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_boolean arg0)))
    | `List ((`String "L_boolean_as_string")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_boolean_as_string arg0)))
    | `List ((`String "L_enum")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_enum arg0)))
    | `List ((`String "L_date")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_date arg0)))
    | `List ((`String "L_duration")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Cnl_t.cnl_literal") arg0) >>=
        ((fun arg0  -> Ok (L_duration arg0)))
    | _ -> Error "Cnl_t.cnl_literal")[@ocaml.warning "-A"])
and (event_name_to_yojson : event_name -> Yojson.Safe.json) =
  (( fun x  -> `String x)[@ocaml.warning
     "-A"])
and (event_name_of_yojson :
       Yojson.Safe.json -> event_name error_or)
  =
  ((
    function
    | `String x -> Ok x
    | _ -> Error "Cnl_t.event_name")[@ocaml.warning "-A"])
and (variable_name_to_yojson : variable_name -> Yojson.Safe.json) =
  (( fun x  -> `String x)[@ocaml.warning
     "-A"])
and (variable_name_of_yojson :
       Yojson.Safe.json -> variable_name error_or)
  =
  ((
    function
    | `String x -> Ok x
    | _ -> Error "Cnl_t.variable_name")[@ocaml.warning "-A"])
and (field_name_to_yojson : field_name -> Yojson.Safe.json) =
  (( fun x  -> `String x)[@ocaml.warning
     "-A"])
and (field_name_of_yojson :
       Yojson.Safe.json -> field_name error_or)
  =
  ((
    function
    | `String x -> Ok x
    | _ -> Error "Cnl_t.field_name")[@ocaml.warning "-A"])
and (cnl_unop_to_yojson : cnl_unop -> Yojson.Safe.json) =
  ((
    function
    | Op_not  -> `List [`String "Op_not"]
    | Op_toString  -> `List [`String "Op_toString"])[@ocaml.warning "-A"])
and (cnl_unop_of_yojson :
       Yojson.Safe.json -> cnl_unop error_or)
  =
  ((
    function
    | `List ((`String "Op_not")::[]) -> Ok Op_not
    | `List ((`String "Op_toString")::[]) -> Ok Op_toString
    | _ -> Error "Cnl_t.cnl_unop")[@ocaml.warning "-A"])
and (cnl_binop_to_yojson : cnl_binop -> Yojson.Safe.json) =
  ((
    function
    | Op_eq  -> `List [`String "Op_eq"]
    | Op_ne  -> `List [`String "Op_ne"]
    | Op_lt  -> `List [`String "Op_lt"]
    | Op_le  -> `List [`String "Op_le"]
    | Op_gt  -> `List [`String "Op_gt"]
    | Op_ge  -> `List [`String "Op_ge"]
    | Op_and  -> `List [`String "Op_and"]
    | Op_or  -> `List [`String "Op_or"]
    | Op_plus  -> `List [`String "Op_plus"]
    | Op_minus  -> `List [`String "Op_minus"]
    | Op_mult  -> `List [`String "Op_mult"]
    | Op_div  -> `List [`String "Op_div"]
    | Op_mod  -> `List [`String "Op_mod"]
    | Op_pow  -> `List [`String "Op_pow"]
    | Op_concat  -> `List [`String "Op_concat"]
    | Op_during  -> `List [`String "Op_during"])[@ocaml.warning "-A"])
and (cnl_binop_of_yojson :
       Yojson.Safe.json -> cnl_binop error_or)
  =
  ((
    function
    | `List ((`String "Op_eq")::[]) -> Ok Op_eq
    | `List ((`String "Op_ne")::[]) -> Ok Op_ne
    | `List ((`String "Op_lt")::[]) -> Ok Op_lt
    | `List ((`String "Op_le")::[]) -> Ok Op_le
    | `List ((`String "Op_gt")::[]) -> Ok Op_gt
    | `List ((`String "Op_ge")::[]) -> Ok Op_ge
    | `List ((`String "Op_and")::[]) -> Ok Op_and
    | `List ((`String "Op_or")::[]) -> Ok Op_or
    | `List ((`String "Op_plus")::[]) -> Ok Op_plus
    | `List ((`String "Op_minus")::[]) -> Ok Op_minus
    | `List ((`String "Op_mult")::[]) -> Ok Op_mult
    | `List ((`String "Op_div")::[]) -> Ok Op_div
    | `List ((`String "Op_mod")::[]) -> Ok Op_mod
    | `List ((`String "Op_pow")::[]) -> Ok Op_pow
    | `List ((`String "Op_concat")::[]) -> Ok Op_concat
    | `List ((`String "Op_during")::[]) -> Ok Op_during
    | _ -> Error "Cnl_t.cnl_binop")[@ocaml.warning "-A"])
and (cnl_aggop_to_yojson : cnl_aggop -> Yojson.Safe.json) =
  ((
    function
    | A_total  -> `List [`String "A_total"]
    | A_avg  -> `List [`String "A_avg"])[@ocaml.warning "-A"])
and (cnl_aggop_of_yojson :
       Yojson.Safe.json -> cnl_aggop error_or)
  =
  ((
    function
    | `List ((`String "A_total")::[]) -> Ok A_total
    | `List ((`String "A_avg")::[]) -> Ok A_avg
    | _ -> Error "Cnl_t.cnl_aggop")[@ocaml.warning "-A"])
[@@@ocaml.text " {6. CNL}"]
type cnl_kind =
  | K_expr of (event_name* field_name) option
  | K_actn
  | K_evnt
  | K_cond
  | K_actns
  | K_actns_closed
  | K_rule
type cnl_ast =
  | Cnl_expr of cnl_expr
  | Cnl_actn of cnl_action
  | Cnl_evnt of cnl_event
  | Cnl_cond of cnl_cond
  | Cnl_actns of cnl_actions
  | Cnl_rule of cnl_rule
