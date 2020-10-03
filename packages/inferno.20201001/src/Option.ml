(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

let iter f o =
  match o with
  | None ->
      ()
  | Some x ->
      f x

let fold f o accu =
  match o with
  | None ->
      accu
  | Some x ->
      f x accu

let map f o =
  match o with
  | None ->
      None
  | Some x ->
      Some (f x)

let multiply m o1 o2 =
  match o1, o2 with
  | None, o
  | o, None ->
      o
  | Some x1, Some x2 ->
      Some (m x1 x2)

