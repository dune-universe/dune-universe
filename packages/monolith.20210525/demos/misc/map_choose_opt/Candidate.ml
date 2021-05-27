(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

(* This file defines which candidate implementation of maps is tested. *)

include Candidate2

(* Define the type [map]. *)

type map =
  int t

(* Define the function [check] to do nothing. *)

let check (_reference : Reference.map) (_candidate : map) =
  ()

(* An intentional bug in [choose_opt]. *)

let choose_opt m =
  let result = choose_opt m in
  match result with
  | Some (14, v) ->
      Some (14, v+1)
  | Some (11, v) ->
      Some (12, v)
  | _ ->
      result
