(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Ppxlib
module Helpers = Common.Helpers
module Info = Helpers.Info
module Error = Common.Error
module T = Types_helper
module P = Common.Ast_helpers.Pattern
module PP = Common.Pp
module REC = Recursives_helpers

type ty = string

type env = { ty : ty; mutual_types : ty list; recursives_types : ty list }

let env ty = { ty; mutual_types = []; recursives_types = [] }

(** [default_env env td] create a default environment from [td] if [env] is absent *)
let default_env env td =
  Option.value
    ~default:
      {
        ty = td.ptype_name.txt;
        mutual_types = [];
        recursives_types = REC.get_recursives_type_declarations [ td ];
      }
    env

(** [is_rec env] looks for [env.ty] inside [env.recursives_types] *)
let is_rec env = List.mem env.ty env.recursives_types

(** [extract_args loc params] extract type parameters

    {[
    type 'a t
    (* args are ['a] *)

    type 'foo * 'oof
    (* args are ['foo; 'oof] *)
    ]} *)
let extract_args ~loc params =
  let to_pat (ct, _) =
    match ct.ptyp_desc with
    | Ptyp_var s -> P.ppat_var ~loc @@ T.name s
    | _ ->
        Error.case_unsupported
          ~loc
          ~case:"type parameters are not all Ptyp_var"
          ()
  in
  List.map to_pat params

let rec from_core_type ~loc ~env ct =
  Option.value
    (Attributes.arb ct)
    ~default:
      (match ct.ptyp_desc with
      | Ptyp_constr ({ txt = ty; _ }, []) ->
          T.from_longident
            ~loc
            ~recursives_types:env.recursives_types
            ~mutual_types:env.mutual_types
            ty
      | Ptyp_constr ({ txt = x; _ }, args) ->
          let f =
            (* TODO: recursives_types and mutual_types empty is weird *)
            T.from_longident ~loc ~recursives_types:[] ~mutual_types:[] x
          in
          let args = List.map (from_core_type ~loc ~env) args in
          T.constr_type ~loc ~f ~args ()
      | Ptyp_tuple xs -> from_tuple ~loc ~env xs
      | Ptyp_var s ->
          (* TODO: they should be optional parameters *)
          T.Primitive.from_string ~loc ~recursives_types:[] ~mutual_types:[] s
      | Ptyp_variant (rws, _, _) -> from_ptyp_variant ~loc ~env rws
      | Ptyp_arrow (_, left, right) -> from_arrow ~loc ~env (left, right)
      | _ ->
          Error.location_error
            ~loc:ct.ptyp_loc
            ~msg:"This type is not supported yet"
            ())

and from_arrow ~loc ~env (left, right) =
  let f = T.observable ~loc in
  let rec arrow_to_list x : expression list * expression =
    match x.ptyp_desc with
    | Ptyp_arrow (_, left, right) ->
        let (acc, x) = arrow_to_list right in
        (f left :: acc, x)
    | _ -> ([], from_core_type ~loc ~env x)
  in
  let (obs, arb) = arrow_to_list right in
  let obs = f left :: obs in

  T.fun_nary ~loc obs arb

and from_ptyp_variant ~loc ~env rws =
  (* Transforms a row_field to the pair (variant name, arbitraries) *)
  let to_expr f rw =
    let w = Attributes.weight rw.prf_attributes in
    match rw.prf_desc with
    | Rtag ({ txt; _ }, _, cts) -> `RTag (txt, w, List.map f cts)
    | Rinherit ct -> `RInh (w, f ct)
  in

  let f = from_core_type ~loc ~env in

  if is_rec env then
    let is_leave x = not @@ REC.is_recursive_row_field ~loc env.ty x in
    let leaves =
      List.filter_map
        (fun rw -> if is_leave rw then Option.some @@ to_expr f rw else None)
        rws
      |> T.variants ~loc ~ty:env.ty
    in
    let nodes = List.map (to_expr f) rws |> T.variants ~loc ~ty:env.ty in
    T.tree' ~loc ~leaves ~nodes ()
  else List.map (to_expr f) rws |> T.variants ~loc ~ty:env.ty

and from_type_kind ~loc ~env = function
  | Ptype_record xs -> Option.some @@ from_record ~loc ~env xs
  | Ptype_variant xs -> Option.some @@ from_variant ~loc ~env xs
  | _ -> None

and from_record ~loc ~env label_decls =
  let gens =
    List.map (fun x -> from_core_type ~loc ~env x.pld_type) label_decls
  in
  T.record ~loc ~gens label_decls

and from_tuple ~loc ~env cts =
  let gens = List.map (from_core_type ~loc ~env) cts in
  T.tuple ~loc gens

and from_variant ~loc ~env xs =
  if is_rec env then
    let is_leave x =
      not @@ REC.is_recursive_constructor_declaration ~loc env.ty x
    in
    let leaves =
      List.filter_map
        (fun x ->
          if is_leave x then Option.some @@ from_constructor_decl ~loc ~env x
          else None)
        xs
    in
    let nodes = List.map (fun x -> from_constructor_decl ~loc ~env x) xs in
    T.tree ~loc ~leaves ~nodes ()
  else List.map (from_constructor_decl ~loc ~env) xs |> T.constructors ~loc

and from_constructor_decl ~loc ~env x =
  let kname = x.pcd_name.txt in
  let f ~kargs = T.constructor ~loc ~kname ~kargs () in
  let constr =
    match x.pcd_args with
    | Pcstr_tuple [] | Pcstr_record [] -> T.constructor ~loc ~kname ()
    | Pcstr_tuple xs ->
        let gens = List.map (from_core_type ~loc ~env) xs in
        let kargs = T.tuple' ~loc gens in
        f ~kargs
    | Pcstr_record xs ->
        let gens = List.map (fun x -> from_core_type ~loc ~env x.pld_type) xs in
        let kargs = T.record' ~loc ~gens xs in
        f ~kargs
  in
  (Attributes.weight x.pcd_attributes, constr)

let from_type_declaration ~loc ?env td =
  let env = default_env env td in
  let is_rec = List.mem env.ty env.recursives_types in
  let args = extract_args ~loc td.ptype_params in

  let arb_from_type_kind = from_type_kind ~loc ~env td.ptype_kind in
  let body =
    match (arb_from_type_kind, td.ptype_manifest) with
    | (Some x, _) -> x
    | (None, Some ct) -> from_core_type ~loc ~env ct
    | _ -> assert false
    (* I don't know at that point if invariants forbids this *)
  in
  T.gen ~loc ~is_rec ~args ~ty:env.ty body

let from_type_declarations ~loc xs =
  let tys = List.map (fun x -> x.ptype_name.txt) xs in

  let env =
    {
      ty = "";
      mutual_types = tys;
      recursives_types = REC.get_recursives_type_declarations xs;
    }
  in

  let gens =
    List.map
      (fun x ->
        let ty = x.ptype_name.txt in
        let env = { env with ty } in
        from_type_declaration ~loc ~env x)
      xs
  in

  T.gens ~loc ~tys ~gens ()
