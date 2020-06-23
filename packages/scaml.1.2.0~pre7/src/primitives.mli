(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Michelson

val primitives : 
  (string * 
   (bool (* pure or not *)
    * int 
    * (loc:Location.t -> Type.t -> Opcode.t list -> Opcode.t list)))
    list

val contract' : string -> loc:Location.t -> Type.t -> Opcode.t list -> Opcode.t list
