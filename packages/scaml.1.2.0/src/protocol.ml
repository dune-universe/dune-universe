(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                     Copyright 2020  DaiLambda, Inc.                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Ocaml_conv.Default

type t = int * int
[@@deriving conv{ocaml}]

let to_string (maj,min) = Printf.sprintf "%d.%d" maj min

let default = (6, 0)  (* Carthage *)

let must_be_a_version = Error "attribute type error: must be a float like 7.0"

let parse s =
  match String.split_on_char '.' s with
  | [maj; min] ->
      begin match int_of_string maj, int_of_string min with
        | exception _ -> must_be_a_version
        | maj, min -> Ok (maj, min)
      end
  | _ -> must_be_a_version

         
