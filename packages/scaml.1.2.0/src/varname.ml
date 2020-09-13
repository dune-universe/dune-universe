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

(* Variable name inference *)

open Michelson.Type

let infer def t =
  let rec infer t =
    match t.tyannot with
    | Some s ->
        (* type annotation may contain invalid chars for idents *)
        Some (String.uncapitalize_ascii
              @@ String.map (function
                  | ('a'..'z' | 'A'..'Z' | '0'..'9' as c) -> c
                  | _ -> '_') s)
    | None ->
        match t.desc with
        | TyString -> Some "s"
        | TyNat -> Some "n"
        | TyInt -> Some "i"
        | TyBytes -> Some "bytes"
        | TyBool -> Some "b"
        | TyUnit -> Some "unit"
        | TyList t ->
            begin match infer t with
              | None -> Some "lst"
              | Some n -> Some (n ^ "s")
            end
        | TyPair _ -> None
        | TyOption (_, t) ->
            begin match infer t with
              | None -> Some "opt"
              | Some n -> Some (n ^ "opt")
            end
        | TyOr _ -> None
        | TySet t ->
            begin match infer t with
              | Some n -> Some (n ^ "_set")
              | None -> Some "_set"
            end
        | TyMap (kt,vt) ->
            begin match infer kt, infer vt with
              | Some k, Some t -> Some (k ^ "_" ^ t ^ "_map")
              | _ -> Some "map"
            end
        | TyBigMap (kt,vt) ->
            begin match infer kt, infer vt with
              | Some k, Some t -> Some (k ^ "_" ^ t ^ "_bmap")
              | _ -> Some "bmap"
            end
        | TyMutez -> Some "tz"
        | TyKeyHash -> Some "kh"
        | TyTimestamp -> Some "tm"
        | TyAddress -> Some "adrs"
        | TyChainID -> Some "chainid"

        | TyKey -> Some "key"
        | TySignature -> Some "sg"
        | TyOperation -> Some "op"
        | TyContract t ->
            begin match infer t with
              | Some t -> Some (t ^ "_cntr")
              | None -> Some "cntr"
            end
        | TyLambda _ -> Some "f"
        | TyNever -> Some "never"
  in
  match infer t with
  | Some s -> s
  | None -> def

let create def ty = Ident.create_local @@ infer def ty
