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

(***************************)
(** {6. Shallow Iterators} *)
(***************************)

(** {8. node} *)

let node_sh_map_fold
      (f: 'a -> 'b -> 'c * 'b)
      (node: 'a node)
      (acc: 'b)
  : 'c node * 'b =
  begin match node with
  | N_undefined id ->
      N_undefined id, acc
  | N_filled (id, x) ->
      let x, acc = f x acc in
      N_filled (id, x), acc
  | N_rejected (id, x) ->
      let x, acc = f x acc in
      N_rejected (id, x), acc
  | N_accepted x ->
      let x, acc = f x acc in
      N_accepted x, acc
  end

let node_sh_map
      (f: 'a -> 'b)
      (node: 'a node)
  : 'b node =
  let node, () = node_sh_map_fold (fun x () -> f x, ()) node () in
  node

let node_sh_fold
      (f: 'a -> 'b -> 'b)
      (node: 'a node)
      (acc: 'b)
  : 'b =
  let _, acc = node_sh_map_fold (fun x acc -> x, f x acc) node acc in
  acc

let node_list_sh_map_fold
      (f_elem: 'a -> 'b -> 'c * 'b)
      (f_closed: unit node -> 'b -> unit node * 'b)
      (l: 'a node_list)
      (acc: 'b)
  : 'c node_list * 'b =
  let rev_l, acc =
    List.fold_left
      (fun (rev_l, acc) elem ->
         let elem, acc = f_elem elem acc in
         elem::rev_l, acc)
      ([], acc) l.list_elems
  in
  let closed, acc = f_closed l.list_closed acc in
  { list_elems = List.rev rev_l;
    list_closed = closed; }, acc

let node_list_sh_map
      (f_elem: 'a -> 'b)
      (f_closed: unit node -> unit node)
      (node: 'a node_list)
  : 'b node_list =
  let node, () =
    node_list_sh_map_fold
      (fun x () -> f_elem x, ())
      (fun x () -> f_closed x, ())
      node ()
  in
  node

let node_list_sh_fold
      (f_elem: 'a -> 'b -> 'b)
      (f_closed: unit node -> 'b -> 'b)
      (node: 'a node_list)
      (acc: 'b)
  : 'b =
  let _, acc =
    node_list_sh_map_fold
      (fun x acc -> x, f_elem x acc)
      (fun x acc -> x, f_closed x acc)
      node acc
  in
  acc


(** {8. rule} *)

let rule_sh_map_fold
      (f_evnt: cnl_event -> 'a -> cnl_event * 'a)
      (f_cond: cnl_cond -> 'a -> cnl_cond * 'a)
      (f_actns: cnl_actions -> 'a -> cnl_actions * 'a)
      (rule: cnl_rule)
      (acc: 'a)
  : cnl_rule * 'a =
  let node, acc =
    node_sh_map_fold
      (fun desc acc ->
         let evnt, acc = f_evnt desc.rule_evnt acc in
         let cond, acc = f_cond desc.rule_cond acc in
         let actns, acc = f_actns desc.rule_actns acc in
         { rule_evnt = evnt;
           rule_cond = cond;
           rule_actns = actns; }, acc)
      rule.rule_node acc
  in
  { rule with rule_node = node; }, acc

let rule_sh_map
      (f_evnt: cnl_event -> cnl_event)
      (f_cond: cnl_cond -> cnl_cond)
      (f_actns: cnl_actions -> cnl_actions)
      (rule: cnl_rule)
  : cnl_rule =
  let rule, () =
    rule_sh_map_fold
      (fun x () -> f_evnt x, ())
      (fun x () -> f_cond x, ())
      (fun x () -> f_actns x, ())
      rule ()
  in
  rule

let rule_sh_fold
      (f_evnt: cnl_event -> 'a -> 'a)
      (f_cond: cnl_cond -> 'a -> 'a)
      (f_actns: cnl_actions -> 'a -> 'a)
      (rule: cnl_rule)
      (acc: 'a)
  : 'a =
  let _, acc =
    rule_sh_map_fold
      (fun x acc -> x, f_evnt x acc)
      (fun x acc -> x, f_cond x acc)
      (fun x acc -> x, f_actns x acc)
      rule acc
  in
  acc


(** {8. event} *)

(** This function is useless. It is present only for symetry reason. *)
let evnt_sh_map_fold
      (f: unit -> 'a -> unit * 'a)
      (evnt: cnl_event)
      (acc: 'a)
  : cnl_event * 'a =
  evnt, acc

let evnt_sh_map
      (f: unit -> unit)
      (evnt: cnl_event)
  : cnl_event =
  let evnt, () = evnt_sh_map_fold (fun x () -> f x, ()) evnt () in
  evnt

let evnt_sh_fold
      (f: unit -> 'a -> 'a)
      (evnt: cnl_event)
      (acc: 'a)
  : 'a =
  let _, acc = evnt_sh_map_fold (fun x acc -> x, f x acc) evnt acc in
  acc


(** {8. cond} *)

let cond_sh_map_fold
      (f_expr: cnl_expr -> 'a -> cnl_expr * 'a)
      (cond: cnl_cond)
      (acc: 'a)
  : cnl_cond * 'a =
  let node, acc =
    node_sh_map_fold
      (fun desc acc ->
         begin match desc with
         | C_no_condition ->
             C_no_condition, acc
         | C_condition e ->
             let e, acc = f_expr e acc in
             C_condition e, acc
         end)
      cond.cond_node acc in
  { cond with cond_node = node; }, acc

let cond_sh_map
      (f_expr: cnl_expr -> cnl_expr)
      (cond: cnl_cond)
  : cnl_cond =
  let cond, () = cond_sh_map_fold (fun x () -> f_expr x, ()) cond () in
  cond

let cond_sh_fold
      (f_expr: cnl_expr -> 'a -> 'a)
      (cond: cnl_cond)
      (acc: 'a)
  : 'a =
  let _, acc = cond_sh_map_fold (fun x acc -> x, f_expr x acc) cond acc in
  acc


(** {8. actions} *)

let actns_sh_map_fold
      (f_actn: cnl_action -> 'a -> cnl_action * 'a)
      (f_closed: unit node -> 'a -> unit node * 'a)
      (actns: cnl_actions)
      (acc: 'a)
  : cnl_actions * 'a =
  let f_desc desc acc =
    node_list_sh_map_fold f_actn f_closed desc acc
  in
  let node, acc = node_sh_map_fold f_desc actns.actns_node acc in
  { actns with actns_node = node; }, acc

let actns_sh_map
      (f_actn: cnl_action -> cnl_action)
      (f_closed: unit node -> unit node)
      (actns: cnl_actions)
  : cnl_actions =
  let actns, () =
    actns_sh_map_fold
      (fun x () -> f_actn x, ())
      (fun x () -> f_closed x, ())
      actns ()
  in
  actns

let actns_sh_fold
      (f_actn: cnl_action -> 'a -> 'a)
      (f_closed: unit node -> 'a -> 'a)
      (actns: cnl_actions)
      (acc: 'a)
  : 'a =
  let _, acc =
    actns_sh_map_fold
      (fun x acc -> x, f_actn x acc)
      (fun x acc -> x, f_closed x acc)
      actns acc
  in
  acc


(** {8. action} *)

let actn_sh_map_fold
      (f_expr: cnl_expr -> 'a -> cnl_expr * 'a)
      (actn: cnl_action)
      (acc: 'a)
  : cnl_action * 'a =
  let f_desc desc acc =
    begin match desc with
    | A_print e ->
        let e, acc = f_expr e acc in
        A_print e, acc
    | A_emit e ->
        let e, acc = f_expr e acc in
        A_emit e, acc
    | A_define (x, e) ->
        let e, acc = f_expr e acc in
        A_define (x, e), acc
    | A_set (fld, x, e) ->
        let e, acc = f_expr e acc in
        A_set (fld, x, e), acc
    end
  in
  let node, acc = node_sh_map_fold f_desc actn.actn_node acc in
  { actn with actn_node = node }, acc

let actns_sh_map
      (f_expr: cnl_expr -> cnl_expr)
      (actn: cnl_action)
  : cnl_action =
  let actn, () = actn_sh_map_fold (fun x () -> f_expr x, ()) actn () in
  actn

let actn_sh_fold
      (f_expr: cnl_expr -> 'a -> 'a)
      (actn: cnl_action)
      (acc: 'a)
  : 'a =
  let _, acc = actn_sh_map_fold (fun x acc -> x, f_expr x acc) actn acc in
  acc

(** {8. expr} *)

let expr_sh_map_fold
      (f_expr: cnl_expr -> 'a -> cnl_expr * 'a)
      (expr: cnl_expr)
      (acc: 'a)
  : cnl_expr * 'a =
  let f_desc expr_desc acc =
    begin match expr_desc with
    | E_lit x -> E_lit x, acc
    | E_var x -> E_var x, acc
    | E_get (e, x) ->
        let e, acc = f_expr e acc in
        E_get (e, x), acc
    | E_agg (op, e, x) ->
        let e, acc = f_expr e acc in
        E_agg (op, e, x), acc
    | E_unop (op, e) ->
        let e, acc = f_expr e acc in
        E_unop (op, e), acc
    | E_binop (op, e1, e2) ->
        let e1, acc = f_expr e1 acc in
        let e2, acc = f_expr e2 acc in
        E_binop (op, e1, e2), acc
    | E_error err ->
        E_error err, acc
    | E_this x ->
        E_this x, acc
    | E_new (x, l) ->
        let rev_l, acc =
          List.fold_left
            (fun (rev_l, acc) (y, e) ->
               let e, acc = f_expr e acc in
               ((y, e) :: rev_l, acc))
            ([], acc) l
        in
        E_new (x, List.rev rev_l), acc
    end
  in
  let node, acc =
    node_sh_map_fold f_desc expr.expr_node acc
  in
  { expr with expr_node = node; }, acc

let expr_sh_map
      (f_expr: cnl_expr -> cnl_expr)
      (expr: cnl_expr)
  : cnl_expr =
  let expr, () = expr_sh_map_fold (fun x () -> f_expr x, ()) expr () in
  expr

let expr_sh_fold
      (f_expr: cnl_expr -> 'a -> 'a)
      (expr: cnl_expr)
      (acc: 'a)
  : 'a =
  let _, acc = expr_sh_map_fold (fun x acc -> x, f_expr x acc) expr acc in
  acc


(************************)
(** {6. Deep Iterators} *)
(************************)

(** {8. Iterators over nodes}
    These iterators implements a prefix traversal of the ASTs.
*)

type 'acc map_fold_over_node_fun = {
  poly_map_fold_fun : 'a. cnl_kind -> 'a node -> 'acc -> 'a node * 'acc;
  rule_map_fold_fun : (cnl_rule_desc node -> 'acc -> cnl_rule_desc node * 'acc);
  evnt_map_fold_fun : (cnl_evnt_desc node -> 'acc -> cnl_evnt_desc node * 'acc);
  cond_map_fold_fun : (cnl_cond_desc node -> 'acc -> cnl_cond_desc node * 'acc);
  actns_map_fold_fun : (cnl_actns_desc node -> 'acc -> cnl_actns_desc node * 'acc);
  actn_map_fold_fun : (cnl_actn_desc node -> 'acc -> cnl_actn_desc node * 'acc);
  expr_map_fold_fun : (cnl_expr_desc node -> 'acc -> cnl_expr_desc node * 'acc);
}

type map_over_node_fun = {
  poly_map_fun : 'a. cnl_kind -> 'a node -> 'a node;
  rule_map_fun : (cnl_rule_desc node -> cnl_rule_desc node);
  evnt_map_fun : (cnl_evnt_desc node -> cnl_evnt_desc node);
  cond_map_fun : (cnl_cond_desc node -> cnl_cond_desc node);
  actns_map_fun : (cnl_actns_desc node -> cnl_actns_desc node);
  actn_map_fun : (cnl_actn_desc node -> cnl_actn_desc node);
  expr_map_fun : (cnl_expr_desc node -> cnl_expr_desc node);
}

type 'acc fold_over_node_fun = {
  poly_fold_fun : 'a. cnl_kind -> 'a node -> 'acc -> 'acc;
  rule_fold_fun : (cnl_rule_desc node -> 'acc -> 'acc);
  evnt_fold_fun : (cnl_evnt_desc node -> 'acc -> 'acc);
  cond_fold_fun : (cnl_cond_desc node -> 'acc -> 'acc);
  actns_fold_fun : (cnl_actns_desc node -> 'acc -> 'acc);
  actn_fold_fun : (cnl_actn_desc node -> 'acc -> 'acc);
  expr_fold_fun : (cnl_expr_desc node -> 'acc -> 'acc);
}


(** {8. Default Iterators} *)

let id_map_fold_over_node_fun =
  let id node acc = (node, acc) in
  { poly_map_fold_fun = (fun kind node acc -> (node, acc));
    rule_map_fold_fun = id;
    evnt_map_fold_fun = id;
    cond_map_fold_fun = id;
    actns_map_fold_fun = id;
    actn_map_fold_fun = id;
    expr_map_fold_fun = id; }

let id_map_over_node_fun =
  let id node = node in
  { poly_map_fun = (fun kind node -> node);
    rule_map_fun = id;
    evnt_map_fun = id;
    cond_map_fun = id;
    actns_map_fun = id;
    actn_map_fun = id;
    expr_map_fun = id; }

let id_fold_over_node_fun =
  let id node acc = acc in
  { poly_fold_fun = (fun kind node acc -> acc);
    rule_fold_fun = id;
    evnt_fold_fun = id;
    cond_fold_fun = id;
    actns_fold_fun = id;
    actn_fold_fun = id;
    expr_fold_fun = id; }

let map_fold_over_node_fun_of_map_over_node_fun f =
  { poly_map_fold_fun = (fun kind node acc -> (f.poly_map_fun kind node, acc));
    rule_map_fold_fun = (fun node acc -> (f.rule_map_fun node, acc));
    evnt_map_fold_fun = (fun node acc -> (f.evnt_map_fun node, acc));
    cond_map_fold_fun = (fun node acc -> (f.cond_map_fun node, acc));
    actns_map_fold_fun = (fun node acc -> (f.actns_map_fun node, acc));
    actn_map_fold_fun = (fun node acc -> (f.actn_map_fun node, acc));
    expr_map_fold_fun = (fun node acc -> (f.expr_map_fun node, acc)); }

let map_fold_over_node_fun_of_fold_over_node_fun f =
  { poly_map_fold_fun = (fun kind node acc -> (node, f.poly_fold_fun kind node acc));
    rule_map_fold_fun = (fun node acc -> (node, f.rule_fold_fun node acc));
    evnt_map_fold_fun = (fun node acc -> (node, f.evnt_fold_fun node acc));
    cond_map_fold_fun = (fun node acc -> (node, f.cond_fold_fun node acc));
    actns_map_fold_fun = (fun node acc -> (node, f.actns_fold_fun node acc));
    actn_map_fold_fun = (fun node acc -> (node, f.actn_fold_fun node acc));
    expr_map_fold_fun = (fun node acc -> (node, f.expr_fold_fun node acc)); }


(** {8. Expr} *)

let rec expr_dp_map_fold_over_nodes
          (f: 'a map_fold_over_node_fun)
          (expr: cnl_expr)
          (acc: 'a)
  : cnl_expr * 'a =
  let node, acc = f.poly_map_fold_fun (K_expr expr.expr_field) expr.expr_node acc in
  let node, acc = f.expr_map_fold_fun node acc in
  expr_sh_map_fold
    (fun e acc -> expr_dp_map_fold_over_nodes f e acc)
    { expr with expr_node = node } acc

let expr_dp_map_over_nodes
      (f: map_over_node_fun)
      (expr: cnl_expr)
  : cnl_expr
  =
  let node, _ =
    expr_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_map_over_node_fun f)
      expr ()
  in
  node

let expr_dp_fold_over_nodes
      (f: 'a fold_over_node_fun)
      (expr: cnl_expr)
      (acc: 'a)
  : 'a
  =
  let _, acc =
    expr_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_fold_over_node_fun f)
      expr acc
  in
  acc

(** {8. Event} *)

let evnt_dp_map_fold_over_nodes
      (f: 'a map_fold_over_node_fun)
      (evnt: cnl_event)
      (acc: 'a)
  : cnl_event * 'a =
  let node, acc = f.poly_map_fold_fun K_evnt evnt.evnt_node acc in
  let node, acc = f.evnt_map_fold_fun node acc in
  evnt_sh_map_fold
    (fun () acc -> (), acc)
    { evnt with evnt_node = node } acc

let evnt_dp_map_over_nodes
      (f: map_over_node_fun)
      (evnt: cnl_event)
  : cnl_event
  =
  let node, _ =
    evnt_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_map_over_node_fun f)
      evnt ()
  in
  node

let evnt_dp_fold_over_nodes
      (f: 'a fold_over_node_fun)
      (evnt: cnl_event)
      (acc: 'a)
  : 'a
  =
  let _, acc =
    evnt_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_fold_over_node_fun f)
      evnt acc
  in
  acc

(** {8. Cond} *)

let cond_dp_map_fold_over_nodes
      (f: 'a map_fold_over_node_fun)
      (cond: cnl_cond)
      (acc: 'a)
  : cnl_cond * 'a
  =
  let node, acc = f.poly_map_fold_fun K_cond cond.cond_node acc in
  let node, acc = f.cond_map_fold_fun node acc in
  cond_sh_map_fold
    (expr_dp_map_fold_over_nodes f)
    { cond with cond_node = node } acc

let cond_dp_map_over_nodes
      (f: map_over_node_fun)
      (cond: cnl_cond)
  : cnl_cond
  =
  let node, _ =
    cond_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_map_over_node_fun f)
      cond ()
  in
  node

let cond_dp_fold_over_nodes
      (f: 'a fold_over_node_fun)
      (cond: cnl_cond)
      (acc: 'a)
  : 'a
  =
  let _, acc =
    cond_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_fold_over_node_fun f)
      cond acc
  in
  acc


(** {8. Action} *)

let actn_dp_map_fold_over_nodes
      (f: 'a map_fold_over_node_fun)
      (actn: cnl_action)
      (acc: 'a)
  : cnl_action * 'a
  =
  let node, acc = f.poly_map_fold_fun K_actn actn.actn_node acc in
  let node, acc = f.actn_map_fold_fun node acc in
  actn_sh_map_fold
    (expr_dp_map_fold_over_nodes f)
    { actn with actn_node = node } acc

let actn_dp_map_over_nodes
      (f: map_over_node_fun)
      (actn: cnl_action)
  : cnl_action
  =
  let node, _ =
    actn_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_map_over_node_fun f)
      actn ()
  in
  node

let actn_dp_fold_over_nodes
      (f: 'a fold_over_node_fun)
      (actn: cnl_action)
      (acc: 'a)
  : 'a
  =
  let _, acc =
    actn_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_fold_over_node_fun f)
      actn acc
  in
  acc


(** {8. Actions}*)

let actns_dp_map_fold_over_nodes
      (f: 'a map_fold_over_node_fun)
      (actns: cnl_actions)
      (acc: 'a)
  : cnl_actions * 'a
  =
  let node, acc = f.poly_map_fold_fun K_actns actns.actns_node acc in
  let node, acc = f.actns_map_fold_fun node acc in
  actns_sh_map_fold
    (actn_dp_map_fold_over_nodes f)
    (f.poly_map_fold_fun K_actns_closed)
    { actns with actns_node = node } acc

let actns_dp_map_over_nodes
      (f: map_over_node_fun)
      (actns: cnl_actions)
  : cnl_actions
  =
  let node, _ =
    actns_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_map_over_node_fun f)
      actns ()
  in
  node

let actns_dp_fold_over_nodes
      (f: 'a fold_over_node_fun)
      (actns: cnl_actions)
      (acc: 'a)
  : 'a
  =
  let _, acc =
    actns_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_fold_over_node_fun f)
      actns acc
  in
  acc


(** {8. Rule} *)

let rule_dp_map_fold_over_nodes
      (f: 'a map_fold_over_node_fun)
      (rule: cnl_rule)
      (acc: 'a)
  : cnl_rule * 'a
  =
  let node, acc = f.poly_map_fold_fun K_rule rule.rule_node acc in
  let node, acc = f.rule_map_fold_fun node acc in
  rule_sh_map_fold
    (evnt_dp_map_fold_over_nodes f)
    (cond_dp_map_fold_over_nodes f)
    (actns_dp_map_fold_over_nodes f)
    { rule with rule_node = node } acc

let rule_dp_map_over_nodes
      (f: map_over_node_fun)
      (rule: cnl_rule)
  : cnl_rule
  =
  let node, _ =
    rule_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_map_over_node_fun f)
      rule ()
  in
  node

let rule_dp_fold_over_nodes
      (f: 'a fold_over_node_fun)
      (rule: cnl_rule)
      (acc: 'a)
  : 'a
  =
  let _, acc =
    rule_dp_map_fold_over_nodes
      (map_fold_over_node_fun_of_fold_over_node_fun f)
      rule acc
  in
  acc


(** {8. CNL} *)

let cnl_dp_map_fold_over_nodes
      (f: 'a map_fold_over_node_fun)
      (cnl: cnl_ast)
      (acc: 'a)
  : cnl_ast * 'a
  =
  begin match cnl with
  | Cnl_expr expr ->
      let expr, acc = expr_dp_map_fold_over_nodes f expr acc in
      Cnl_expr expr, acc
  | Cnl_actn actn ->
      let actn, acc = actn_dp_map_fold_over_nodes f actn acc in
      Cnl_actn actn, acc
  | Cnl_evnt evnt ->
      let evnt, acc = evnt_dp_map_fold_over_nodes f evnt acc in
      Cnl_evnt evnt, acc
  | Cnl_cond cond ->
      let cond, acc = cond_dp_map_fold_over_nodes f cond acc in
      Cnl_cond cond, acc
  | Cnl_actns actns ->
      let actns, acc = actns_dp_map_fold_over_nodes f actns acc in
      Cnl_actns actns, acc
  | Cnl_rule rule ->
      let rule, acc = rule_dp_map_fold_over_nodes f rule acc in
      Cnl_rule rule, acc
  end
