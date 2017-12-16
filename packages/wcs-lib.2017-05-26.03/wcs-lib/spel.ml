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


(** Spel data structure constructors *)

open Spel_t
open Wcs_t
open Spel_util

(** {6 parsers} *)

let of_string s = Spel_parse.expr_from_string s

let of_text s = Spel_parse.quoted_expr_from_string s

(** {6 printers} *)

let to_string = Spel_print.to_string

let to_text = Spel_print.to_text


(** {6 from OCaml types} *)

let entity entity_def ?value () =
  begin match value with
  | None ->
      mk_expr (E_entity (entity_def.e_def_entity, None))
  | Some value_def ->
      let value = value_def.e_val_value in
      if List.mem value (List.map (fun ev -> ev.e_val_value) entity_def.e_def_values)
      then
        mk_expr (E_entity (entity_def.e_def_entity, Some value))
      else
        Log.error "Spel builder" None ("Undefined entity: " ^  value)
  end

let intent intent_def =
  mk_expr (E_intent intent_def.i_def_intent)

let bool b =
  mk_expr (E_lit (L_boolean b))

let int n =
  mk_expr (E_lit (L_int n))

let string s =
  mk_expr (E_lit (L_string s))


(** {6 expression constructors} *)

let prop e x =
  mk_expr (E_prop (e, x))

let prop_catch e x =
  mk_expr (E_prop_catch (e, x))

let get e1 e2  =
  mk_expr (E_get (e1, e2))

let list l =
  mk_expr (E_list l)

let new_array t l1 l2 =
  mk_expr (E_new_array (t, l1, l2))

let new_ t l =
  mk_expr (E_new (t, l))

let call e m l =
  mk_expr (E_call (e, m ,l ))

let call_catch e m l =
  mk_expr (E_call_catch (e, m ,l ))

let op op l =
  mk_expr (E_op (op, l))

let eq e1 e2 =
  op Op_eq [ e1; e2; ]

let ne e1 e2 =
  op Op_ne [ e1; e2; ]

let lt e1 e2 =
  op Op_lt [ e1; e2; ]

let gt e1 e2 =
  op Op_gt [ e1; e2; ]

let ge e1 e2 =
  op Op_ge [ e1; e2; ]

let not e =
  op Op_not [ e; ]

let and_ e1 e2 =
  op Op_and [ e1; e2; ]

let or_ e1 e2 =
  op Op_or [ e1; e2; ]

let plus e1 e2 =
  op Op_plus [ e1; e2; ]

let minus e1 e2 =
  op Op_minus [ e1; e2; ]

let uminus e =
  op Op_uminus [ e; ]

let mult e1 e2 =
  op Op_mult [ e1; e2; ]

let div e1 e2 =
  op Op_div [ e1; e2; ]

let mod_ e1 e2 =
  op Op_mod [ e1; e2; ]

let concat l =
  op Op_concat l

let to_string e =
  op Op_toString [ e; ]

let conditional e1 e2 e3 =
  mk_expr (E_conditional (e1, e2, e3))

let ident v =
  mk_expr (E_ident v)

(** {6 other constructors} *)

let anything_else =
  mk_expr E_anything_else

let context =
  mk_expr E_context

let conversation_start =
  mk_expr E_conversation_start

let entitites =
  mk_expr E_entities

let input =
  mk_expr E_input

let intents =
  mk_expr E_intents

let output =
  mk_expr E_output

let variable v =
  mk_expr (E_variable (v, None))


(** {6 Spel checker *)

let spel_check s =
  let e = of_string s in
  Spel_print.to_string e

