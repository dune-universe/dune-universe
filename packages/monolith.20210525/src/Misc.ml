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

(* Postincrementation. *)

let[@inline] postincrement k =
  let x = !k in k := x + 1; x

(* Printing a number in a compact format. *)

open Printf

let summarize n =
  if n < 1000 then
    sprintf "%3d" n
  else if n < 1000000 then
    sprintf "%3dK" (n / 1000)
  else if n < 1000000000 then
    sprintf "%3dM" (n / 1000000)
  else
    sprintf "%3dG" (n / 1000000000)

(* Creating a directory (and its parents) if they do not already exist. *)

let mkdirp dirname =
  ignore (Sys.command (sprintf "mkdir -p \"%s\"" (String.escaped dirname)))
