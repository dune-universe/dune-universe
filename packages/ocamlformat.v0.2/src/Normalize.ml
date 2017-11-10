(**********************************************************************)
(*                                                                    *)
(*                            OCamlFormat                             *)
(*                                                                    *)
(*  Copyright (c) 2017-present, Facebook, Inc.  All rights reserved.  *)
(*                                                                    *)
(*  This source code is licensed under the MIT license found in the   *)
(*  LICENSE file in the root directory of this source tree.           *)
(*                                                                    *)
(**********************************************************************)

(** Normalize abstract syntax trees *)

open Migrate_ast
open Asttypes
open Parsetree
open Ast_helper

let mapper =
  (* remove locations *)
  let location _ _ = Location.none in
  (* sort attributes *)
  let attributes (m: Ast_mapper.mapper) atrs =
    List.sort ~cmp:Poly.compare
      (Ast_mapper.default_mapper.attributes m atrs)
  in
  let expr (m: Ast_mapper.mapper) exp =
    let {pexp_desc; pexp_loc; pexp_attributes} = exp in
    match pexp_desc with
    (* convert [(f x) y] to [f x y] *)
    | Pexp_apply ({pexp_desc= Pexp_apply (f, xs)}, ys) ->
        m.expr m
          (Exp.apply ~loc:pexp_loc ~attrs:pexp_attributes f (xs @ ys))
    (* convert [~- int_const] to [-int_const] *)
    | Pexp_apply
        ( { pexp_desc= Pexp_ident {txt= Lident "~-"}
          ; pexp_loc
          ; pexp_attributes= atrs0 }
        , [ ( _
            , { pexp_desc= Pexp_constant Pconst_integer (lit, suf)
              ; pexp_attributes= atrs1 } ) ] ) ->
        m.expr m
          (Exp.constant ~loc:pexp_loc ~attrs:(atrs0 @ atrs1)
             (Pconst_integer ("-" ^ lit, suf)))
    (* convert [~-] ident to [-] *)
    | Pexp_apply
        ( ( { pexp_desc= Pexp_ident ({txt= Lident "~-"} as lid)
            ; pexp_loc
            ; pexp_attributes } as e1 )
        , e1N ) ->
        m.expr m
          (Exp.apply ~loc:pexp_loc ~attrs:pexp_attributes
             {e1 with pexp_desc= Pexp_ident {lid with txt= Lident "-"}} e1N)
    (* convert [(c1; c2); c3] to [c1; (c2; c3)] *)
    | Pexp_sequence
        ({pexp_desc= Pexp_sequence (e1, e2); pexp_attributes= []}, e3) ->
        m.expr m
          (Exp.sequence e1 (Exp.sequence ~attrs:pexp_attributes e2 e3))
    | _ -> Ast_mapper.default_mapper.expr m exp
  in
  let pat (m: Ast_mapper.mapper) pat =
    let {ppat_desc; ppat_loc= loc1; ppat_attributes= attrs1} = pat in
    (* normalize nested or patterns *)
    match ppat_desc with
    | Ppat_or
        ( pat1
        , { ppat_desc= Ppat_or (pat2, pat3)
          ; ppat_loc= loc2
          ; ppat_attributes= attrs2 } ) ->
        m.pat m
          (Pat.or_ ~loc:loc1 ~attrs:attrs1
             (Pat.or_ ~loc:loc2 ~attrs:attrs2 pat1 pat2)
             pat3)
    | _ -> Ast_mapper.default_mapper.pat m pat
  in
  let value_binding (m: Ast_mapper.mapper) vb =
    let { pvb_pat= {ppat_desc; ppat_loc; ppat_attributes}
        ; pvb_expr
        ; pvb_loc
        ; pvb_attributes } =
      vb
    in
    match ppat_desc with
    (* convert [let (x : t) = e] to [let x = (e : t)] *)
    | Ppat_constraint (p0, t0) ->
        m.value_binding m
          (Vb.mk ~loc:pvb_loc ~attrs:pvb_attributes p0
             (Exp.constraint_ ~loc:ppat_loc ~attrs:ppat_attributes pvb_expr
                t0))
    | _ -> Ast_mapper.default_mapper.value_binding m vb
  in
  { Ast_mapper.default_mapper with
    location; attributes; expr; pat; value_binding }


let impl = map_structure mapper

let intf = map_signature mapper

let equal_impl ast1 ast2 = Poly.equal (impl ast1) (impl ast2)

let equal_intf ast1 ast2 = Poly.equal (intf ast1) (intf ast2)
