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

(* An intentional bug in [choose]. *)

let choose m =
  match choose m with
  | (14, v) ->
      (14, v+1)
  | (11, v) ->
      (12, v)
  | (7, _) ->
      failwith "Empty" (* raise the wrong exception! *)
  | result ->
      result
