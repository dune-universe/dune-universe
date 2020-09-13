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

module List = struct
  include List

  let rec mark_last = function
    | [] -> []
    | [x] -> [x,true]
    | x::xs -> (x,false)::mark_last xs

  let rec last = function
    | [] -> None
    | [x] -> Some x
    | _::xs -> last xs
end

module Result = struct
  include Result

  let rec fold_leftM f acc = function
    | [] -> return acc
    | x::xs ->
        f acc x >>= fun acc' ->
        fold_leftM f acc' xs
end

module Longident = struct
  include Longident

  let rec to_string = function
    | Lident s -> s
    | Ldot (lid, s) -> to_string lid ^ "." ^ s
    | Lapply (t1,t2) -> to_string t1 ^ "(" ^ to_string t2 ^ ")"
end

module Ident = struct
  include Ident

  let is_stdlib i = name i = "Stdlib" && persistent i
  let is_scaml i = name i = "SCaml" && persistent i

  let dummy = Ident.create_local "_dummy_"
end

module Path = struct
  include Path

  let rec xname = function
    | Pident id -> Ident.unique_name id
    | Pdot (t, s) -> Printf.sprintf "%s.%s" (xname t) s
    | Papply (t1,t2) -> xname t1 ^ "(" ^ xname t2 ^ ")"

  let is_stdlib = function
    | Pdot (Pident id, s) when Ident.is_stdlib id -> Some s
    | _ -> None

  let rec is_scaml = function
    | Pdot (Pident id, s) when Ident.is_scaml id -> Some s
    | Pdot (p, s) ->
        begin match is_scaml p with
          | None -> None
          | Some m -> Some (m ^ "." ^ s)
        end
    | _ -> None

  let is_scaml_dot n = function
    | Pdot (Pident id, s) when Ident.is_scaml id -> s = n
    | _ -> false
end

module Location = struct
  include Location
  let ghost t = { t with loc_ghost= true }
end

let errorf n ~loc fmt =
  Location.raise_errorf ~loc ("[ESCaml%03d] " ^^ fmt) n

let errorf_var_not_found fmt = errorf 010 fmt
let errorf_stdlib        fmt = errorf 020 fmt
let errorf_type_expr     fmt = errorf 100 fmt
let errorf_constant      fmt = errorf 200 fmt
let errorf_big_map       fmt = errorf 210 fmt
let errorf_entry         fmt = errorf 300 fmt
let errorf_entry_typing  fmt = errorf 310 fmt
let errorf_link          fmt = errorf 350 fmt
let errorf_freevar       fmt = errorf 400 fmt
let errorf_self          fmt = errorf 500 fmt
let errorf_contract      fmt = errorf 600 fmt
let errorf_pattern_match fmt = errorf 700 fmt
let errorf_primitive     fmt = errorf 800 fmt
let errorf_flags         fmt = errorf 900 fmt
let errorf_attribute     fmt = errorf 910 fmt
let errorf_convert_ident fmt = errorf 920 fmt

let unsupported ~loc fmt =
  Printf.ksprintf (fun s ->
      errorf 0 ~loc "SCaml does not support %s" s) fmt

let internal_error ~loc fmt =
  Printf.ksprintf (fun s ->
      errorf 999 ~loc "SCaml internal error: %s\n%s" s
        Printexc.(raw_backtrace_to_string (get_callstack 20))
    ) fmt

exception Wrapped_OCaml_error of Location.t * string * exn

let () =
  let rec f = function
    | Wrapped_OCaml_error (_, _, (Wrapped_OCaml_error _ as exn)) -> f exn
    | Wrapped_OCaml_error (loc, msg, exn) ->
        Some (
          match Location.error_of_exn exn with
          | Some (`Ok ocaml) ->
              { ocaml with
                main = { loc; txt= (fun ppf -> Format.fprintf ppf "%s" msg) }
              ; sub = ocaml.Location.main :: ocaml.Location.sub
              }
          | _ ->
              { Location.kind= Report_error
              ; main= { loc; txt= (fun ppf -> Format.fprintf ppf "unknown exception: %s" (Printexc.to_string exn)) }
              ; sub= []
              }
        )
    | _ -> None
  in
  Location.register_error_of_exn f

let wrap_ocaml_exn exn n ~loc fmt =
  let open Format in
  let buf = Buffer.create 64 in
  let ppf = formatter_of_buffer buf in
  Misc.Color.set_color_tag_handling ppf;
  kfprintf
    (fun _ ->
      pp_print_flush ppf ();
      let msg = Buffer.contents buf in
      raise (Wrapped_OCaml_error (loc, msg, exn))
    )
    ppf ("[ESCaml%03d] " ^^ fmt) n

let with_time f =
  let t1 = Unix.gettimeofday () in
  let res = f () in
  let t2 = Unix.gettimeofday () in
  res, (t2 -. t1)
