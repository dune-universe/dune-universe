(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers


type t =
  | Const of { name : Name.t; arity : int; scope : Scope.t }
  | Var of { name : Name.t; arity : int; scope : Scope.t; fby : Scope.t option }


let const name arity scope = Const { name; arity; scope }

let var name arity scope fby =  Var { name; arity; scope; fby }


let arity = function
  | Const { arity; _ }
  | Var { arity; _ } -> arity

let name = function
  | Const { name; _ }
  | Var { name; _ } -> name

let is_set rel =
  arity rel = 1
  
let is_nary rel =
  arity rel > 1

let is_const = function
  | Const _ -> true
  | Var _ -> false

let is_var = function
  | Const _ -> false
  | Var _ -> true

let scope = function
  | Const { scope; _}
  | Var { scope; _ } -> scope


let pp ?(print_name = true) out rel =
  let open! Fmtc in             (* shadows const *)
  let pp_name =
    if print_name then (sp **> colon **> nbsp **> Name.pp) else nop
  in
  let pp_def const_or_var name scope fby arity = 
    pp_name out name;
    (styled `Bold @@ string) out const_or_var;
    Format.pp_open_hbox out ();
    string out "«";
    int out arity;
    (sp **> string) out "»";
    Format.pp_open_box out 2;
    (box2 @@ Scope.pp) out scope;
    option ((styled `Bold @@ sp **< const string "then")
            **< sp **< (box2 @@ Scope.pp)) out fby;
    Format.pp_close_box out ();
    Format.pp_close_box out ()
  in
  match rel with
    | Const { name; scope; arity } ->
        pp_def "const" name scope None arity
    | Var { name; scope; fby; arity  } ->
        pp_def "var" name scope fby arity


let must = function
  | Const { scope; _ }
  | Var { scope; _ } ->
      Scope.must scope

let may = function
  | Const { scope; _ }
  | Var { scope; _ } ->
      Scope.may scope

let sup = function
  | Const { scope; _ }
  | Var { scope; _ } ->
      Scope.sup scope

let to_string ?(print_name = true) rel =
  Fmtc.to_to_string (pp ~print_name) rel
