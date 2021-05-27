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

open Monolith

(* -------------------------------------------------------------------------- *)

(* This wrapper maps an integer to a list of integers. *)

(* It fails if the integer is negative. This is a mistake in the wrapper that
   the system should detect. *)

let expand n =
  List.init n (fun i -> i)

let () =
  dprintf "let expand n = List.init n (fun i -> i);;\n"

let appearance =
  constant "expand"

let expand spec =
  map_outof expand (expand, appearance) spec

(* -------------------------------------------------------------------------- *)

(* Declare our operation. *)

let () =

  let spec = expand (semi_open_interval (-16) 16) ^> int in
  declare "length" spec List.length List.length

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
