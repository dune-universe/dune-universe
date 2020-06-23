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

open Spotlib.Spot
open Ocaml_conv.Default
open Untyped

type mode =
  | Compile
  | ConvertAll
  | ConvertSingleValue of string
  | ConvertSingleType of string
  | Revert of string

and t = 
  { iml_optimization : bool
  ; iml_pattern_match : bool
  ; scaml_debug : bool
  ; scaml_time : bool
  ; scaml_mode : mode option
  ; scaml_noscamlib : bool
  ; dump_iml : bool
  } [@@deriving conv{ocaml}]

let pp = Camlon.Ocaml.format_with ocaml_of_t

let set_mode t m =
  match t.scaml_mode with
  | None -> { t with scaml_mode = Some m }
  | Some _ -> failwith "You cannot change SCaml running mode twice"

let eval flags (k, v) =
  let must_be_a_bool () = Error "attribute type error: must be a bool" in
  match String.concat "." & Longident.flatten_exn k, v with
  | "iml_optimization", `Bool b -> Ok { flags with iml_optimization= b }
  | "iml_optimization", _ -> must_be_a_bool ()
  | "iml_pattern_match", `Bool b -> Ok { flags with iml_pattern_match= b }
  | "iml_pattern_match", _ -> must_be_a_bool ()
  | "scaml_debug", `Bool b -> Ok { flags with scaml_debug= b }
  | "scaml_debug", _ -> must_be_a_bool ()
  | "scaml_time", `Bool b -> Ok { flags with scaml_time= b }
  | "scaml_time", _ -> must_be_a_bool ()
(*
  | "scaml_convert", `Unit -> set_mode flags Convert
  | "scaml_convert", _ -> must_be_a_unit ()
  | "scaml_revert", `String s -> set_mode flags (Revert s)
  | "scaml_revert", _ -> must_be_a_unit ()
*)
  | "scaml_noscamlib", `Bool b -> Ok { flags with scaml_noscamlib= b }
  | "scaml_noscamlib", _ -> must_be_a_bool ()
  | "dump_iml", `Bool b -> Ok { flags with dump_iml= b }
  | "dump_iml", _ -> must_be_a_bool ()
  | n, _ -> Error (Printf.sprintf "Unknown attribute %s" n)

let flags = ref 
    { iml_optimization  = true
    ; iml_pattern_match = true 
    ; scaml_debug       = begin try ignore (Sys.getenv "SCAML_DEBUG"); true with _ -> false end 
    ; scaml_time        = false
    ; scaml_mode        = None
    ; scaml_noscamlib   = false
    ; dump_iml          = begin try ignore (Sys.getenv "SCAML_DUMP_IML"); true with _ -> false end 
    }

let update f = flags := f !flags

let with_flags f g = 
  let org = !flags in
  flags := f !flags;
  let res = g () in
  flags := org;
  res
  
let if_debug f = if !flags.scaml_debug then f ()
let if_time f = if !flags.scaml_time then f ()

