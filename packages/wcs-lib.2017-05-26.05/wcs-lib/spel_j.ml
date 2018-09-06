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

open Spel_t

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

let (>>=) x f =
  match x with Ok x -> f x | (Error _) as x -> x

let (>|=) x f =
  x >>= fun x -> Ok (f x)

let rec map_bind f acc xs =
  match xs with
  | x :: xs -> f x >>= fun x -> map_bind f (x :: acc) xs
  | [] -> Ok (List.rev acc)

type 'a error_or = ('a, string) result

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
let rec (spel_type_to_yojson : spel_type -> Json_t.safe) =
  ((
    function
    | T_string  -> `List [`String "T_string"]
    | T_int  -> `List [`String "T_int"]
    | T_real  -> `List [`String "T_real"]
    | T_boolean  -> `List [`String "T_boolean"]
    | T_object  -> `List [`String "T_object"])[@ocaml.warning "-A"])
and (spel_type_of_yojson :
       Json_t.safe -> spel_type error_or)
  =
  ((
    function
    | `List ((`String "T_string")::[]) -> Ok T_string
    | `List ((`String "T_int")::[]) -> Ok T_int
    | `List ((`String "T_real")::[]) -> Ok T_real
    | `List ((`String "T_boolean")::[]) -> Ok T_boolean
    | `List ((`String "T_object")::[]) -> Ok T_object
    | _ -> Error "Spel_t.spel_type")[@ocaml.warning "-A"])
let rec (literal_to_yojson : literal -> Json_t.safe) =
  ((
    function
    | L_string arg0 ->
        `List [`String "L_string"; ((fun x  -> `String x)) arg0]
    | L_int arg0 -> `List [`String "L_int"; ((fun x  -> `Int x)) arg0]
    | L_real arg0 -> `List [`String "L_real"; ((fun x  -> `Float x)) arg0]
    | L_boolean arg0 ->
        `List [`String "L_boolean"; ((fun x  -> `Bool x)) arg0]
    | L_null  -> `List [`String "L_null"])[@ocaml.warning "-A"])
and (literal_of_yojson :
       Json_t.safe -> literal error_or)
  =
  ((
    function
    | `List ((`String "L_string")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Spel_t.literal") arg0) >>=
        ((fun arg0  -> Ok (L_string arg0)))
    | `List ((`String "L_int")::arg0::[]) ->
        ((function
         | `Int x -> Ok x
         | _ -> Error "Spel_t.literal") arg0) >>=
        ((fun arg0  -> Ok (L_int arg0)))
    | `List ((`String "L_real")::arg0::[]) ->
        ((function
         | `Int x -> Ok (float_of_int x)
         | `Intlit x -> Ok (float_of_string x)
         | `Float x -> Ok x
         | _ -> Error "Spel_t.literal") arg0) >>=
        ((fun arg0  -> Ok (L_real arg0)))
    | `List ((`String "L_boolean")::arg0::[]) ->
        ((function
         | `Bool x -> Ok x
         | _ -> Error "Spel_t.literal") arg0) >>=
        ((fun arg0  -> Ok (L_boolean arg0)))
    | `List ((`String "L_null")::[]) -> Ok L_null
    | _ -> Error "Spel_t.literal")[@ocaml.warning "-A"])
let rec (op_to_yojson : op -> Json_t.safe) =
  ((
    function
    | Op_eq  -> `List [`String "Op_eq"]
    | Op_ne  -> `List [`String "Op_ne"]
    | Op_lt  -> `List [`String "Op_lt"]
    | Op_le  -> `List [`String "Op_le"]
    | Op_gt  -> `List [`String "Op_gt"]
    | Op_ge  -> `List [`String "Op_ge"]
    | Op_not  -> `List [`String "Op_not"]
    | Op_and  -> `List [`String "Op_and"]
    | Op_or  -> `List [`String "Op_or"]
    | Op_plus  -> `List [`String "Op_plus"]
    | Op_minus  -> `List [`String "Op_minus"]
    | Op_uminus  -> `List [`String "Op_uminus"]
    | Op_mult  -> `List [`String "Op_mult"]
    | Op_div  -> `List [`String "Op_div"]
    | Op_mod  -> `List [`String "Op_mod"]
    | Op_concat  -> `List [`String "Op_concat"]
    | Op_toString  -> `List [`String "Op_toString"])[@ocaml.warning "-A"])
and (op_of_yojson :
       Json_t.safe -> op error_or)
  =
  ((
    function
    | `List ((`String "Op_eq")::[]) -> Ok Op_eq
    | `List ((`String "Op_ne")::[]) -> Ok Op_ne
    | `List ((`String "Op_lt")::[]) -> Ok Op_lt
    | `List ((`String "Op_le")::[]) -> Ok Op_le
    | `List ((`String "Op_gt")::[]) -> Ok Op_gt
    | `List ((`String "Op_ge")::[]) -> Ok Op_ge
    | `List ((`String "Op_not")::[]) -> Ok Op_not
    | `List ((`String "Op_and")::[]) -> Ok Op_and
    | `List ((`String "Op_or")::[]) -> Ok Op_or
    | `List ((`String "Op_plus")::[]) -> Ok Op_plus
    | `List ((`String "Op_minus")::[]) -> Ok Op_minus
    | `List ((`String "Op_uminus")::[]) -> Ok Op_uminus
    | `List ((`String "Op_mult")::[]) -> Ok Op_mult
    | `List ((`String "Op_div")::[]) -> Ok Op_div
    | `List ((`String "Op_mod")::[]) -> Ok Op_mod
    | `List ((`String "Op_concat")::[]) -> Ok Op_concat
    | `List ((`String "Op_toString")::[]) -> Ok Op_toString
    | _ -> Error "Spel_t.op")[@ocaml.warning "-A"])
let rec (expression_to_yojson : expression -> Json_t.safe) =
  ((
    fun x  ->
      let fields = [] in
      let fields =
        ("expr_text",
         ((function | None  -> `Null | Some x -> ((fun x  -> `String x)) x)
            x.expr_text))
        :: fields in
      let fields =
        ("expr_loc", ((fun x  -> location_to_yojson x) x.expr_loc)) ::
        fields in
      let fields =
        ("expr_desc",
         ((fun x  -> expression_desc_to_yojson x) x.expr_desc))
        :: fields in
      `Assoc fields)[@ocaml.warning "-A"])
and (expression_of_yojson :
       Json_t.safe -> expression error_or)
  =
  ((
    function
    | `Assoc xs ->
        let rec loop xs ((arg0,arg1,arg2) as _state) =
          match xs with
          | ("expr_desc",x)::xs ->
              loop xs
                (((fun x  -> expression_desc_of_yojson x) x), arg1, arg2)
          | ("expr_loc",x)::xs ->
              loop xs (arg0, ((fun x  -> location_of_yojson x) x), arg2)
          | ("expr_text",x)::xs ->
              loop xs
                (arg0, arg1,
                 ((function
                  | `Null -> Ok None
                  | x ->
                      ((function
                       | `String x -> Ok x
                       | _ -> Error "Spel_t.expression.expr_text")
                         x)
                      >>= ((fun x  -> Ok (Some x)))) x))
          | [] ->
              arg2 >>=
              ((fun arg2  ->
                 arg1 >>=
                 (fun arg1  ->
                    arg0 >>=
                    (fun arg0  ->
                       Ok
                         {
                           expr_desc = arg0;
                           expr_loc = arg1;
                           expr_text = arg2
                         }))))
          | _::xs -> Error "Spel_t.expression" in
        loop xs
          ((Error "Spel_t.expression.expr_desc"),
           (Error "Spel_t.expression.expr_loc"),
           (Error "Spel_t.expression.expr_text"))
    | _ -> Error "Spel_t.expression")[@ocaml.warning "-A"])
and (expression_desc_to_yojson : expression_desc -> Json_t.safe) =
  ((
    function
    | E_lit arg0 ->
        `List [`String "E_lit"; ((fun x  -> literal_to_yojson x)) arg0]
    | E_prop (arg0,arg1) ->
        `List
          [`String "E_prop";
           ((fun x  -> expression_to_yojson x)) arg0;
           ((fun x  -> `String x)) arg1]
    | E_prop_catch (arg0,arg1) ->
        `List
          [`String "E_prop_catch";
           ((fun x  -> expression_to_yojson x)) arg0;
           ((fun x  -> `String x)) arg1]
    | E_get (arg0,arg1) ->
        `List
          [`String "E_get";
           ((fun x  -> expression_to_yojson x)) arg0;
           ((fun x  -> expression_to_yojson x)) arg1]
    | E_list arg0 ->
        `List
          [`String "E_list";
           ((fun x  -> `List (List.map (fun x  -> expression_to_yojson x) x)))
             arg0]
    | E_new_array (arg0,arg1,arg2) ->
        `List
          [`String "E_new_array";
           ((fun x  -> spel_type_to_yojson x)) arg0;
           ((fun x  ->
              `List
                (List.map
                   (function
                   | None  -> `Null
                   | Some x -> ((fun x  -> `Int x)) x) x))) arg1;
           ((function
            | None  -> `Null
            | Some x ->
                ((fun x  ->
                   `List (List.map (fun x  -> expression_to_yojson x) x)))
                  x)) arg2]
    | E_new (arg0,arg1) ->
        `List
          [`String "E_new";
           ((fun x  -> `String x)) arg0;
           ((fun x  -> `List (List.map (fun x  -> expression_to_yojson x) x)))
             arg1]
    | E_call (arg0,arg1,arg2) ->
        `List
          [`String "E_call";
           ((function
            | None  -> `Null
            | Some x -> ((fun x  -> expression_to_yojson x)) x)) arg0;
           ((fun x  -> `String x)) arg1;
           ((fun x  -> `List (List.map (fun x  -> expression_to_yojson x) x)))
             arg2]
    | E_call_catch (arg0,arg1,arg2) ->
        `List
          [`String "E_call_catch";
           ((function
            | None  -> `Null
            | Some x -> ((fun x  -> expression_to_yojson x)) x)) arg0;
           ((fun x  -> `String x)) arg1;
           ((fun x  -> `List (List.map (fun x  -> expression_to_yojson x) x)))
             arg2]
    | E_op (arg0,arg1) ->
        `List
          [`String "E_op";
           ((fun x  -> op_to_yojson x)) arg0;
           ((fun x  -> `List (List.map (fun x  -> expression_to_yojson x) x)))
             arg1]
    | E_conditional (arg0,arg1,arg2) ->
        `List
          [`String "E_conditional";
           ((fun x  -> expression_to_yojson x)) arg0;
           ((fun x  -> expression_to_yojson x)) arg1;
           ((fun x  -> expression_to_yojson x)) arg2]
    | E_ident arg0 ->
        `List [`String "E_ident"; ((fun x  -> `String x)) arg0]
    | E_anything_else  -> `List [`String "E_anything_else"]
    | E_context  -> `List [`String "E_context"]
    | E_conversation_start  -> `List [`String "E_conversation_start"]
    | E_entities  -> `List [`String "E_entities"]
    | E_input  -> `List [`String "E_input"]
    | E_intents  -> `List [`String "E_intents"]
    | E_output  -> `List [`String "E_output"]
    | E_variable arg0 ->
        `List
          [`String "E_variable";
           ((fun (arg0,arg1)  ->
              `List
                [((fun x  -> `String x)) arg0;
                 ((function
                  | None  -> `Null
                  | Some x -> ((fun x  -> `String x)) x)) arg1])) arg0]
    | E_intent arg0 ->
        `List [`String "E_intent"; ((fun x  -> `String x)) arg0]
    | E_entity arg0 ->
        `List
          [`String "E_entity";
           ((fun (arg0,arg1)  ->
              `List
                [((fun x  -> `String x)) arg0;
                 ((function
                  | None  -> `Null
                  | Some x -> ((fun x  -> `String x)) x)) arg1])) arg0]
    | E_error arg0 -> `List [`String "E_error"; `String arg0])[@ocaml.warning "-A"])
and (expression_desc_of_yojson :
       Json_t.safe ->
     expression_desc error_or)
  =
  ((
    function
    | `List ((`String "E_lit")::arg0::[]) ->
        ((fun x  -> literal_of_yojson x) arg0) >>=
        ((fun arg0  -> Ok (E_lit arg0)))
    | `List ((`String "E_prop")::arg0::arg1::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Spel_t.expression_desc") arg1) >>=
        ((fun arg1  ->
           ((fun x  -> expression_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_prop (arg0, arg1)))))
    | `List ((`String "E_prop_catch")::arg0::arg1::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Spel_t.expression_desc") arg1) >>=
        ((fun arg1  ->
           ((fun x  -> expression_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_prop_catch (arg0, arg1)))))
    | `List ((`String "E_get")::arg0::arg1::[]) ->
        ((fun x  -> expression_of_yojson x) arg1) >>=
        ((fun arg1  ->
           ((fun x  -> expression_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_get (arg0, arg1)))))
    | `List ((`String "E_list")::arg0::[]) ->
        ((function
         | `List xs -> map_bind (fun x  -> expression_of_yojson x) [] xs
         | _ -> Error "Spel_t.expression_desc") arg0) >>=
        ((fun arg0  -> Ok (E_list arg0)))
    | `List ((`String "E_new_array")::arg0::arg1::arg2::[]) ->
        ((function
         | `Null -> Ok None
         | x ->
             ((function
              | `List xs ->
                  map_bind (fun x  -> expression_of_yojson x) [] xs
              | _ -> Error "Spel_t.expression_desc") x) >>=
             ((fun x  -> Ok (Some x)))) arg2)
        >>=
        ((fun arg2  ->
           ((function
            | `List xs ->
                map_bind
                  (function
                  | `Null -> Ok None
                  | x ->
                      ((function
                       | `Int x -> Ok x
                       | _ -> Error "Spel_t.expression_desc")
                         x)
                      >>= ((fun x  -> Ok (Some x)))) [] xs
            | _ -> Error "Spel_t.expression_desc") arg1) >>=
           (fun arg1  ->
              ((fun x  -> spel_type_of_yojson x) arg0) >>=
              (fun arg0  ->
                 Ok (E_new_array (arg0, arg1, arg2))))))
    | `List ((`String "E_new")::arg0::arg1::[]) ->
        ((function
         | `List xs -> map_bind (fun x  -> expression_of_yojson x) [] xs
         | _ -> Error "Spel_t.expression_desc") arg1) >>=
        ((fun arg1  ->
           ((function
            | `String x -> Ok x
            | _ -> Error "Spel_t.expression_desc") arg0)
           >>=
           (fun arg0  -> Ok (E_new (arg0, arg1)))))
    | `List ((`String "E_call")::arg0::arg1::arg2::[]) ->
        ((function
         | `List xs -> map_bind (fun x  -> expression_of_yojson x) [] xs
         | _ -> Error "Spel_t.expression_desc") arg2) >>=
        ((fun arg2  ->
           ((function
            | `String x -> Ok x
            | _ -> Error "Spel_t.expression_desc") arg1) >>=
           (fun arg1  ->
              ((function
               | `Null -> Ok None
               | x ->
                   ((fun x  -> expression_of_yojson x) x) >>=
                   ((fun x  -> Ok (Some x)))) arg0)
              >>=
              (fun arg0  -> Ok (E_call (arg0, arg1, arg2))))))
    | `List ((`String "E_call_catch")::arg0::arg1::arg2::[]) ->
        ((function
         | `List xs -> map_bind (fun x  -> expression_of_yojson x) [] xs
         | _ -> Error "Spel_t.expression_desc") arg2) >>=
        ((fun arg2  ->
           ((function
            | `String x -> Ok x
            | _ -> Error "Spel_t.expression_desc") arg1) >>=
           (fun arg1  ->
              ((function
               | `Null -> Ok None
               | x ->
                   ((fun x  -> expression_of_yojson x) x) >>=
                   ((fun x  -> Ok (Some x)))) arg0)
              >>=
              (fun arg0  -> Ok (E_call_catch (arg0, arg1, arg2))))))
    | `List ((`String "E_op")::arg0::arg1::[]) ->
        ((function
         | `List xs -> map_bind (fun x  -> expression_of_yojson x) [] xs
         | _ -> Error "Spel_t.expression_desc") arg1) >>=
        ((fun arg1  ->
           ((fun x  -> op_of_yojson x) arg0) >>=
           (fun arg0  -> Ok (E_op (arg0, arg1)))))
    | `List ((`String "E_conditional")::arg0::arg1::arg2::[]) ->
        ((fun x  -> expression_of_yojson x) arg2) >>=
        ((fun arg2  ->
           ((fun x  -> expression_of_yojson x) arg1) >>=
           (fun arg1  ->
              ((fun x  -> expression_of_yojson x) arg0) >>=
              (fun arg0  ->
                 Ok (E_conditional (arg0, arg1, arg2))))))
    | `List ((`String "E_ident")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Spel_t.expression_desc") arg0) >>=
        ((fun arg0  -> Ok (E_ident arg0)))
    | `List ((`String "E_anything_else")::[]) -> Ok E_anything_else
    | `List ((`String "E_context")::[]) -> Ok E_context
    | `List ((`String "E_conversation_start")::[]) -> Ok E_conversation_start
    | `List ((`String "E_entities")::[]) -> Ok E_entities
    | `List ((`String "E_input")::[]) -> Ok E_input
    | `List ((`String "E_intents")::[]) -> Ok E_intents
    | `List ((`String "E_output")::[]) -> Ok E_output
    | `List ((`String "E_variable")::arg0::[]) ->
        ((function
         | `List (arg0::arg1::[]) ->
             ((function
              | `Null -> Ok None
              | x ->
                  ((function
                   | `String x -> Ok x
                   | _ -> Error "Spel_t.expression_desc") x) >>=
                  ((fun x  -> Ok (Some x)))) arg1)
             >>=
             ((fun arg1  ->
                ((function
                 | `String x -> Ok x
                 | _ -> Error "Spel_t.expression_desc") arg0)
                >>= (fun arg0  -> Ok (arg0, arg1))))
         | _ -> Error "Spel_t.expression_desc") arg0) >>=
        ((fun arg0  -> Ok (E_variable arg0)))
    | `List ((`String "E_intent")::arg0::[]) ->
        ((function
         | `String x -> Ok x
         | _ -> Error "Spel_t.expression_desc") arg0) >>=
        ((fun arg0  -> Ok (E_intent arg0)))
    | `List ((`String "E_entity")::arg0::[]) ->
        ((function
         | `List (arg0::arg1::[]) ->
             ((function
              | `Null -> Ok None
              | x ->
                  ((function
                   | `String x -> Ok x
                   | _ -> Error "Spel_t.expression_desc") x) >>=
                  ((fun x  -> Ok (Some x)))) arg1)
             >>=
             ((fun arg1  ->
                ((function
                 | `String x -> Ok x
                 | _ -> Error "Spel_t.expression_desc") arg0)
                >>= (fun arg0  -> Ok (arg0, arg1))))
         | _ -> Error "Spel_t.expression_desc") arg0) >>=
        ((fun arg0  -> Ok (E_entity arg0)))
    | `List ((`String "E_error")::(`String arg0)::[]) ->
        ((fun x  -> Ok x) arg0) >>=
        ((fun arg0  -> Ok (E_error arg0)))
    | _ -> Error "Spel_t.expression_desc")[@ocaml.warning "-A"])
