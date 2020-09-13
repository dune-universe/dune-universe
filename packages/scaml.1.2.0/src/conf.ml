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
  ; debug     : bool
  ; time      : bool
  ; mode      : mode
  ; noscamlib : bool
  ; dump_iml  : bool
  ; protocol  : Protocol.t (* Support protocol specific features *)
  } [@@deriving conv{ocaml}]

and opt =
  { op_iml_optimization : bool option
  ; op_debug     : bool option
  ; op_time      : bool option
  ; op_mode      : mode option
  ; op_noscamlib : bool option
  ; op_dump_iml  : bool option
  ; op_protocol  : Protocol.t option
  } [@@deriving conv{ocaml}]

let pp = Camlon.Ocaml.format_with ocaml_of_t
let pp_opt = Camlon.Ocaml.format_with ocaml_of_opt

let default =
  { iml_optimization  = true
  ; debug     = false
  ; time      = false
  ; mode      = Compile
  ; noscamlib = false
  ; dump_iml  = false
  ; protocol  = Protocol.default
  }

let conf = ref default
let get_conf () = !conf

let if_debug f = if !conf.debug then f ()
let if_time f = if !conf.time then f ()
let get_protocol () = !conf.protocol

(* default values by environment variables: executed at program startup *)
let default_debug =
  try ignore (Sys.getenv "SCAML_DEBUG"); true with _ -> false

let default_dump_iml =
  try ignore (Sys.getenv "SCAML_DUMP_IML"); true with _ -> false

let default_time =
  try ignore (Sys.getenv "SCAML_TIME"); true with _ -> false

let unopt o =
  let f def = function
    | None -> def
    | Some x -> x
  in
  { iml_optimization = f true o.op_iml_optimization
  ; debug     = f default_debug o.op_debug
  ; time      = f default_time o.op_time
  ; mode      = f Compile o.op_mode
  ; noscamlib = f false o.op_noscamlib
  ; dump_iml  = f default_dump_iml o.op_dump_iml
  ; protocol  = f Protocol.default o.op_protocol
  }

let none =
  { op_iml_optimization= None
  ; op_debug= None
  ; op_time= None
  ; op_mode= None
  ; op_noscamlib= None
  ; op_dump_iml= None
  ; op_protocol= None
  }

let merge o1 o2 =
  let f n opt1 opt2 = match opt1, opt2 with
    | Some v1, None -> Some v1
    | None, Some v2 -> Some v2
    | None, None -> None
    | Some v1, Some v2 when v1 = v2 -> Some v1
    | Some _, Some _ -> failwith n
  in
  try
    { op_iml_optimization=
        f "iml_optimization" o1.op_iml_optimization o2.op_iml_optimization
    ; op_debug=
        f "debug" o1.op_debug o2.op_debug
    ; op_time=
        f "time" o1.op_time o2.op_time
    ; op_mode=
        f "mode" o1.op_mode o2.op_mode
    ; op_noscamlib=
        f "noscamlib" o1.op_noscamlib o2.op_noscamlib
    ; op_dump_iml=
        f "dump_iml" o1.op_dump_iml o2.op_dump_iml
    ; op_protocol=
        f "protocol" o1.op_protocol o2.op_protocol
    }
  with
  | Failure e ->
      Tools.errorf_flags ~loc:Location.none "Cannot merge conflicting option of %s" e

let eval (k, v) =
  let open Result.Infix in
  let must_be_a_bool = Error "attribute type error: must be a bool" in
  match String.concat "." & Longident.flatten_exn k, v with
  | "iml_optimization", `Bool b -> Ok { none with op_iml_optimization= Some b }
  | "iml_optimization", _ -> must_be_a_bool
  | "debug", `Bool b -> Ok { none with op_debug= Some b }
  | "debug", _ -> must_be_a_bool
  | "time", `Bool b -> Ok { none with op_time= Some b }
  | "time", _ -> must_be_a_bool
  | "noscamlib", `Bool b -> Ok { none with op_noscamlib= Some b }
  | "noscamlib", _ -> must_be_a_bool
  | "dump_iml", `Bool b -> Ok { none with op_dump_iml= Some b }
  | "dump_iml", _ -> must_be_a_bool
  | "protocol", `Constant (Parsetree.Pconst_float (s, None)) ->
      Protocol.parse s >>| fun v ->
      { none with op_protocol= Some v }
  | "protocol", _ ->
      begin match Protocol.parse "dummy" with
        | Error e -> Error e
        | Ok _ -> assert false
      end
  | n, _ -> Error (Printf.sprintf "Unknown attribute %s" n)

let of_attrs attrs =
  List.fold_left (fun op (Location.{txt; loc}, v) ->
      merge op
      & Result.at_Error (fun e ->
          Tools.errorf_attribute ~loc "%s" e)
      & eval (txt, v))
    none attrs

let set_to_conf op = conf := unopt op

let opt = ref none

let with_opt xopt f =
  let org = !opt in
  opt := merge org xopt;
  set_to_conf !opt;
  let res = f () in
  opt := org;
  res

let with_scaml_attrs attrs f = with_opt (of_attrs attrs) f

let get_opt () = !opt
