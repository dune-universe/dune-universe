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
open Json_spel_t
open Spel_j

type ('a, 'b) result = ('a, 'b) Spel_j.result =
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

let rec (json_spel_to_yojson : json_spel -> Json_t.safe) =
  ((
    function
    | `Assoc x ->
        `List
          [`String "Assoc";
           ((fun x  ->
              `List
                (List.map
                   (fun (arg0,arg1)  ->
                      `List
                        [((fun x  -> `String x)) arg0;
                         ((fun x  -> json_spel_to_yojson x)) arg1]) x))) x]
    | `Bool x -> `List [`String "Bool"; ((fun x  -> `Bool x)) x]
    | `Float x -> `List [`String "Float"; ((fun x  -> `Float x)) x]
    | `Int x -> `List [`String "Int"; ((fun x  -> `Int x)) x]
    | `List x ->
        `List
          [`String "List";
           ((fun x  -> `List (List.map (fun x  -> json_spel_to_yojson x) x)))
             x]
    | `Null -> `List [`String "Null"]
    | `Expr x ->
        `List [`String "Expr"; ((fun x  -> expression_to_yojson x)) x])
      [@ocaml.warning "-A"])
and (json_spel_of_yojson :
       Json_t.safe -> json_spel error_or)
  =
  ((
    fun (json : Json_t.safe)  ->
      match json with
      | `List ((`String "Assoc")::x::[]) ->
          ((function
           | `List xs ->
               map_bind
                 (function
                 | `List (arg0::arg1::[]) ->
                     ((fun x  -> json_spel_of_yojson x) arg1) >>=
                     ((fun arg1  ->
                        ((function
                         | `String x -> Ok x
                         | _ -> Error "Spel_t.json_spel") arg0)
                        >>= (fun arg0  -> Ok (arg0, arg1))))
                 | _ -> Error "Spel_t.json_spel") [] xs
           | _ -> Error "Spel_t.json_spel") x) >>=
          ((fun x  -> Ok (`Assoc x)))
      | `List ((`String "Bool")::x::[]) ->
          ((function
           | `Bool x -> Ok x
           | _ -> Error "Spel_t.json_spel") x) >>=
          ((fun x  -> Ok (`Bool x)))
      | `List ((`String "Float")::x::[]) ->
          ((function
           | `Int x -> Ok (float_of_int x)
           | `Intlit x -> Ok (float_of_string x)
           | `Float x -> Ok x
           | _ -> Error "Spel_t.json_spel") x) >>=
          ((fun x  -> Ok (`Float x)))
      | `List ((`String "Int")::x::[]) ->
          ((function
           | `Int x -> Ok x
           | _ -> Error "Spel_t.json_spel") x) >>=
          ((fun x  -> Ok (`Int x)))
      | `List ((`String "List")::x::[]) ->
          ((function
           | `List xs -> map_bind (fun x  -> json_spel_of_yojson x) [] xs
           | _ -> Error "Spel_t.json_spel") x) >>=
          ((fun x  -> Ok (`List x)))
      | `List ((`String "Null")::[]) -> Ok `Null
      | `List ((`String "Expr")::x::[]) ->
          ((fun x  -> expression_of_yojson x) x) >>=
          ((fun x  -> Ok (`Expr x)))
      | _ -> Error "Spel_t.json_spel")[@ocaml.warning "-A"])
