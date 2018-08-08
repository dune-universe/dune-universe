(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Longident

let rec to_string = function
  | Lident s -> s
  | Ldot (t, s) -> to_string t ^ "." ^ s
  | Lapply (t1, t2) -> Printf.sprintf "%s(%s)" (to_string t1) (to_string t2)


