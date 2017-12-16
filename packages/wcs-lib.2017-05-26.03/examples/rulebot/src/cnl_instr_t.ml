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

open Wcs_lib
open Json_t
open Deriving_intf
open Cnl_t

let (>>=) x f =
  match x with Ok x -> f x | (Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> Ok (List.rev acc)

type 'a error_or = ('a, string) result

type cnl_instr =
  | I_repl_expr of int* cnl_expr_desc
  | I_repl_actn of int* cnl_actn_desc
  | I_repl_evnt of int* cnl_evnt_desc
  | I_repl_cond of int* cnl_cond_desc
  | I_repl_actns of int* cnl_actns_desc
  | I_repl_actns_closed of int* bool
  | I_conf_expr of int* bool
  | I_conf_actn of int* bool
  | I_conf_evnt of int* bool
  | I_conf_cond of int* bool
  | I_conf_actns of int* bool
  | I_conf_rule of int* bool
  | I_insr_actn
let rec (cnl_instr_to_yojson : cnl_instr -> Yojson.Safe.json) =
  ((
    function
    | I_repl_expr (arg0,arg1) ->
        `List
          [`String "I_repl_expr";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> cnl_expr_desc_to_yojson x)) arg1]
    | I_repl_actn (arg0,arg1) ->
        `List
          [`String "I_repl_actn";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> cnl_actn_desc_to_yojson x)) arg1]
    | I_repl_evnt (arg0,arg1) ->
        `List
          [`String "I_repl_evnt";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> cnl_evnt_desc_to_yojson x)) arg1]
    | I_repl_cond (arg0,arg1) ->
        `List
          [`String "I_repl_cond";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> cnl_cond_desc_to_yojson x)) arg1]
    | I_repl_actns (arg0,arg1) ->
        `List
          [`String "I_repl_actns";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> cnl_actns_desc_to_yojson x)) arg1]
    | I_repl_actns_closed (arg0,arg1) ->
        `List
          [`String "I_repl_actns_closed";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_conf_expr (arg0,arg1) ->
        `List
          [`String "I_conf_expr";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_conf_actn (arg0,arg1) ->
        `List
          [`String "I_conf_actn";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_conf_evnt (arg0,arg1) ->
        `List
          [`String "I_conf_evnt";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_conf_cond (arg0,arg1) ->
        `List
          [`String "I_conf_cond";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_conf_actns (arg0,arg1) ->
        `List
          [`String "I_conf_actns";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_conf_rule (arg0,arg1) ->
        `List
          [`String "I_conf_rule";
           ((fun x  -> `Int x)) arg0;
           ((fun x  -> `Bool x)) arg1]
    | I_insr_actn  -> `List [`String "I_insr_actn"])[@ocaml.warning "-A"])
and (cnl_instr_of_yojson :
       Yojson.Safe.json -> cnl_instr error_or)
  =
  ((
    function
    | `List ((`String "I_repl_expr")::arg0::arg1::[]) ->
        ((fun x  -> cnl_expr_desc_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_repl_expr (arg0, arg1)))))
    | `List ((`String "I_repl_actn")::arg0::arg1::[]) ->
        ((fun x  -> cnl_actn_desc_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_repl_actn (arg0, arg1)))))
    | `List ((`String "I_repl_evnt")::arg0::arg1::[]) ->
        ((fun x  -> cnl_evnt_desc_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_repl_evnt (arg0, arg1)))))
    | `List ((`String "I_repl_cond")::arg0::arg1::[]) ->
        ((fun x  -> cnl_cond_desc_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_repl_cond (arg0, arg1)))))
    | `List ((`String "I_repl_actns")::arg0::arg1::[]) ->
        ((fun x  -> cnl_actns_desc_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_repl_actns (arg0, arg1)))))
    | `List ((`String "I_repl_actns_closed")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_repl_actns_closed (arg0, arg1)))))
    | `List ((`String "I_conf_expr")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_conf_expr (arg0, arg1)))))
    | `List ((`String "I_conf_actn")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_conf_actn (arg0, arg1)))))
    | `List ((`String "I_conf_evnt")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_conf_evnt (arg0, arg1)))))
    | `List ((`String "I_conf_cond")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_conf_cond (arg0, arg1)))))
    | `List ((`String "I_conf_actns")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_conf_actns (arg0, arg1)))))
    | `List ((`String "I_conf_rule")::arg0::arg1::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Cnl_instr_t.cnl_instr") arg1) >>=
        ((fun arg1  ->
           ((function
            | `Int x -> Ok x
            | _ -> Error "Cnl_instr_t.cnl_instr") arg0) >>=
           (fun arg0  -> Ok (I_conf_rule (arg0, arg1)))))
    | `List ((`String "I_insr_actn")::[]) -> Ok I_insr_actn
    | _ -> Error "Cnl_instr_t.cnl_instr")[@ocaml.warning "-A"])
type cnl_program = cnl_instr list
let rec (cnl_program_to_yojson : cnl_program -> Yojson.Safe.json) =
  ((
    fun x  -> `List (List.map (fun x  -> cnl_instr_to_yojson x) x))
      [@ocaml.warning "-A"])
and (cnl_program_of_yojson :
       Yojson.Safe.json -> cnl_program error_or)
  =
  ((
    function
    | `List xs -> map_bind (fun x  -> cnl_instr_of_yojson x) [] xs
    | _ -> Error "Cnl_instr_t.cnl_program")[@ocaml.warning "-A"])
let focus_of_instr instr =
  match instr with
  | I_repl_expr (focus,_) -> focus
  | I_repl_actn (focus,_) -> focus
  | I_repl_evnt (focus,_) -> focus
  | I_repl_cond (focus,_) -> focus
  | I_repl_actns (focus,_) -> focus
  | I_repl_actns_closed (focus,_) -> focus
  | I_conf_expr (focus,_) -> focus
  | I_conf_actn (focus,_) -> focus
  | I_conf_evnt (focus,_) -> focus
  | I_conf_cond (focus,_) -> focus
  | I_conf_actns (focus,_) -> focus
  | I_conf_rule (focus,_) -> focus
  | I_insr_actn  -> assert false
