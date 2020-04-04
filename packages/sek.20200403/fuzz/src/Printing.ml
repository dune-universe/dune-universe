(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Printf

let pp_int f n =
  fprintf f "%d" n

let pp_bool f n =
  fprintf f "%b" n

let sp_int () n =
  sprintf "%d" n

let sp_bool () n =
  sprintf "%b" n

let pp_option pp_value f = function
  | None ->
      fprintf f "None"
  | Some x ->
      fprintf f "(Some %a)" pp_value x

let sp_option sp_value () = function
  | None ->
      sprintf "None"
  | Some x ->
      sprintf "(Some %a)" sp_value x

let sp_collection left right sp_value () a =
  let b = Buffer.create 32 in
  let out = Buffer.add_string b in
  out left;
  Array.iter (fun x -> out (sp_value () x); out ";") a;
  out right;
  Buffer.contents b

let pp_collection left right pp_value f a =
  let out = output_string f in
  out left;
  Array.iter (fun x -> pp_value f x; out ";") a;
  out right

let sp_array sp_value () a =
  sp_collection "[|" "|]" sp_value () a

let pp_array pp_value f a =
  pp_collection "[|" "|]" pp_value f a

let sp_list sp_value () a =
  sp_collection "[" "]" sp_value () (Array.of_list a)

let pp_list pp_value f a =
  pp_collection "[" "]" pp_value f (Array.of_list a)

let sp_pair sp_value1 sp_value2 () (a1, a2) =
  sprintf "(%a, %a)" sp_value1 a1 sp_value2 a2

let pp_pair pp_value1 pp_value2 f (a1, a2) =
  fprintf f "(%a, %a)" pp_value1 a1 pp_value2 a2
