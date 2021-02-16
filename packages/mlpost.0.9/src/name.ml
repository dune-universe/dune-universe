(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

type t = string

let create prefix =
  let r = ref 0 in
  fun () ->
    incr r;
    prefix ^ string_of_int !r

let node = create "node"

let rec alpha i =
  if 0 <= i && i <= 22 then String.make 1 (Char.chr (Char.code 'a' + i))
  else alpha (i / 22) ^ alpha (i mod 22)

let path =
  let r = ref 0 in
  fun () ->
    incr r;
    "path" ^ alpha !r

let picture =
  let r = ref 0 in
  fun () ->
    incr r;
    "pic" ^ alpha !r

let point =
  let r = ref 0 in
  fun () ->
    incr r;
    "pot" ^ alpha !r

let num =
  let r = ref 0 in
  fun () ->
    incr r;
    "num" ^ alpha !r

let transform =
  let r = ref 0 in
  fun () ->
    incr r;
    "trans" ^ alpha !r
