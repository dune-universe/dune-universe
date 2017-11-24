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
open Cnl_builder

(** Empty rule *)

let empty_init () =
  let evnt = mk_evnt_undefined () in
  let cond = mk_cond_undefined () in
  let actns = mk_actns_undefined () in
  let r0 = mk_rule_f evnt cond actns in
  index_rule r0

let rule_init =
  ref None

let set_rule_init r =
  rule_init := Some r

let rule_init () =
  begin match !rule_init with
  | None -> empty_init ()
  | Some r -> r
  end

(** Partial ASTs *)

let cond_init () : cnl_cond_desc =
  C_condition (mk_expr_undefined ())

let actns_init () : cnl_actns_desc =
  { list_elems = [ mk_actn_undefined () ];
    list_closed = N_undefined None; }

let print_init () : cnl_actn_desc =
  A_print (mk_expr_undefined ())

let emit_init () : cnl_actn_desc =
  A_emit (mk_expr_undefined ())

let define_init vname : cnl_actn_desc =
  A_define (vname,mk_expr_undefined ())

let set_init vname fname : cnl_actn_desc =
  A_set (vname,fname,mk_expr_undefined ())

(**
   when a transaction occurs, called 'the transaction'
   if
    the balance of 'the account' is less than 'Average Risky Account Balance'
   then
   print "aggregate " + 'Average Risky Account Balance' ;
   print "balance" + the balance of 'the account';
   define 'the exception' as a new exception where
        the reason is "The account for " + the email of the customer of 'the account' + " is risky and below the average balance for risky accounts." ,
        the code is "R04" ;
    emit a new authorization response where
        the account is 'the account' ,
        the message is "R04: the account for " + the email of the customer of 'the account' + " is risky and below the average balance for risky accounts.",
        the exception is 'the exception' ,
        the transaction is 'the transaction' ;
*)

let define1 =
  mk_define_f "the exception"
    (mk_new_event_f "exception"
       [("reason",mk_concat_list_f
                    [mk_string_f "The account for ";
                     mk_get_f (mk_get_var_f "customer" "the account") "email";
                     mk_string_f " is risky and below the average balance for risky accounts."]);
        ("code",mk_string_f "R04")])
let emit1 =
  mk_emit_f
    (mk_new_event_f "authorization response"
       [("account",mk_var_f "the account");
        ("message", mk_concat_list_f
                      [mk_string_f "R04: the account for ";
                       mk_get_f (mk_get_var_f "the account" "customer") "email";
                       mk_string_f " is risky and below the average balance for risky accounts."]);
        ("exception",mk_var_f "the exception");
        ("transaction",mk_var_f "the transaction")])
let when1 = ("transaction",Some "the transaction")
let cond1 = C_condition (mk_lt_f (mk_get_var_f "balance" "the account") (mk_expr_f (E_var "Average Risky Account Balance")))
let then1 =
  { list_elems = [mk_print_f (mk_concat_f (mk_string_f "aggregate ") (mk_var_f "Average Risky Account Balance"));
                  mk_print_f (mk_concat_f (mk_string_f "balance") (mk_get_var_f "balance" "the account"));
                  define1;
                  emit1;];
    list_closed = N_filled (None, ()); }

let rule1 = mk_rule_init when1 cond1 then1

(**
   when an airplane event occurs
   then
   define rpmAverage as (
    the average engine rpm of 'the airplane' +
    the rpm of the engine of this airplane event) / 2;
   define pressureAverage as (
    the average engine pressure ratio of 'the airplane' +
    the pressure ratio of the engine of this airplane event) / 2;
   set the average engine rpm of 'the airplane' to rpmAverage;
   set the average engine pressure ratio of 'the airplane' to
    pressureAverage;
*)

let define21 =
  mk_define_f
    "rpmAverage"
    (mk_div_f
       (mk_plus_f
          (mk_get_var_f "average engine rpm" "the airplane")
          (mk_get_f (mk_get_f (mk_this_f "airplane") "engine") "rpm"))
       (mk_int_f 2))
let define22 =
  mk_define_f
    "pressureAverage"
    (mk_div_f
       (mk_plus_f
          (mk_get_var_f "average engine pressure ratio" "the airplane")
          (mk_get_f (mk_get_f (mk_this_f "airplane") "engine") "pressure ratio"))
       (mk_int_f 2))

let setdesc21 = mk_set_desc_f "average engine rpm" "the airplane" (mk_var_f "rpmAverage")
let setdesc22 = mk_set_desc_f "average engine pressure ratio" "the airplane" (mk_var_f "pressureAverage")
let set21 = mk_set_f "average engine rpm" "the airplane" (mk_var_f "rpmAverage")
let set22 = mk_set_f "average engine pressure ratio" "the airplane" (mk_var_f "pressureAverage")

let rule2 =
  mk_rule_init
    ("airplane", None)
    C_no_condition
    { list_elems = [define21;
                    define22;
                    set21;
                    set22;];
      list_closed = N_filled (None, ()); }

(** Table of samples *)
let cnl_samples =
  [ ("rule_init", rule_init ());
    ("rule1un1", index_rule rule1);
    ("rule1", rule_f_to_a rule1);
    ("rule2", rule2); ]

(* Sample expressions *)

let expr1 = mk_expr_undefined () (* XXX TODO XXX *)
(* "expr": { *)
(*   "expr_desc": [ *)
(*     "P_confirmed", *)
(*     [ *)
(*       "E_binop", *)
(*       [ *)
(*         "Op_lt" *)
(*       ], *)
(*       { *)
(*         "expr_desc": [ *)
(*           "P_confirmed", *)
(*           [ *)
(*             "E_prop", *)
(*             { *)
(*               "expr_desc": [ *)
(*                 "P_confirmed", *)
(*                 [ *)
(*                   "E_variable", *)
(*                   "balance" *)
(*                 ] *)
(*               ] *)
(*             }, *)
(*             "the account" *)
(*           ] *)
(*         ] *)
(*       }, *)
(*       { *)
(*         "expr_desc": [ *)
(*           "P_confirmed", *)
(*           [ *)
(*             "E_variable", *)
(*             "Average Risky Account Balance" *)
(*           ] *)
(*         ] *)
(*       } *)
(*     ] *)
(*   ] *)
(* } *)

let expr2 = mk_expr_undefined () (* XXX TODO XXX *)
(* "expr": { *)
(*   "expr_desc": [ *)
(*     "P_filled", *)
(*     13, *)
(*     [ *)
(*       "E_binop", *)
(*       [ *)
(*         "Op_concat" *)
(*       ], *)
(*       { *)
(*         "expr_desc": [ *)
(*           "P_filled", *)
(*           14, *)
(*           [ *)
(*             "E_lit", *)
(*             [ *)
(*               "L_string", *)
(*               "balance" *)
(*             ] *)
(*           ] *)
(*         ] *)
(*       }, *)
(*       { *)
(*         "expr_desc": [ *)
(*           "P_filled", *)
(*           15, *)
(*           [ *)
(*             "E_prop", *)
(*             { *)
(*               "expr_desc": [ *)
(*                 "P_filled", *)
(*                 16, *)
(*                 [ *)
(*                   "E_variable", *)
(*                   "balance" *)
(*                 ] *)
(*               ] *)
(*             }, *)
(*             "the account" *)
(*           ] *)
(*         ] *)
(*       } *)
(*     ] *)
(*   ] *)
(* } *)
