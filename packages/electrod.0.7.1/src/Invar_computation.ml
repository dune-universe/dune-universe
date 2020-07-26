(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

open Containers

type goal_color =
  | Static_prop
  | Primed_prop
  | Invar
  | Init
  | Trans
  | Temporal

let to_string (gc : goal_color) =
  match gc with
  | Static_prop ->
      "Static_prop"
  | Primed_prop ->
      "Primed_prop"
  | Invar ->
      "Invar"
  | Init ->
      "Init"
  | Trans ->
      "Trans"
  | Temporal ->
      "Temporal"


let pp out gc = Fmtc.(pf out "%a" (styled `Yellow string) (to_string gc))

(* Computes the most general color between two colors *)
let max_color c1 c2 =
  match (c1, c2) with
  | Static_prop, Static_prop ->
      Static_prop
  | (Init | Static_prop), (Init | Static_prop) ->
      Init
  | (Primed_prop | Init | Static_prop), (Primed_prop | Init | Static_prop) ->
      Primed_prop
  | (Invar | Static_prop), (Invar | Static_prop) ->
      Invar
  | (Trans | Static_prop), (Trans | Static_prop) ->
      Trans
  | _, _ ->
      Temporal


(* same as max_color, but without Invar nor Trans *)
let max_color_wiwt c1 c2 =
  match (c1, c2) with
  | Static_prop, Static_prop ->
      Static_prop
  | (Init | Static_prop), (Init | Static_prop) ->
      Init
  | (Primed_prop | Init | Static_prop), (Primed_prop | Init | Static_prop) ->
      Primed_prop
  | _, _ ->
      Temporal


let rec remove_always_from_invar (Elo.Fml { node; _ } as fml) =
  let open Elo in
  match node with
  | LUn (G, subfml) ->
      remove_always_from_invar subfml
  | LBin (fml1, And, fml2) ->
      let fml1' = remove_always_from_invar fml1 in
      let fml2' = remove_always_from_invar fml2 in
      lbinary fml1' and_ fml2'
  | _ ->
      fml


let add_always_to_invar (Elo.Fml { node; _ } as fml) =
  let open Elo in
  match node with LUn (G, _) -> fml | _ -> lunary always fml


let is_const (elo : Elo.t) (name : Name.t) =
  assert (Domain.mem name elo.Elo.domain);
  Domain.get_exn name elo.Elo.domain |> Relation.is_const


class ['self] computer (elo : Elo.t) =
  object (_ : 'self)
    inherit ['self] Elo.fold

    method build_fml () f' _ = f'

    method build_Run () blk' = List.fold_left max_color_wiwt Static_prop blk'

    method build_Check () blk' = List.fold_left max_color_wiwt Static_prop blk'

    method build_True () = Static_prop

    method build_False () = Static_prop

    method build_Block () blk_colors =
      List.fold_left max_color Static_prop blk_colors

    method build_FIte () f t e =
      match (f, t, e) with
      | Static_prop, Static_prop, Static_prop ->
          Static_prop
      | (Init | Static_prop), (Init | Static_prop), (Init | Static_prop) ->
          Init
      | ( (Primed_prop | Init | Static_prop)
        , (Primed_prop | Init | Static_prop)
        , (Primed_prop | Init | Static_prop) ) ->
          Primed_prop
      | _, _, _ ->
          Temporal

    method build_Let () __bs' __block' = assert false

    (* SIMPLIFIED *)

    (* quant *)
    method build_Quant () quant' (_, _, range_color) blk_colors =
      let blk_color = List.fold_left max_color_wiwt Static_prop blk_colors in
      let sim_binding_color = max_color_wiwt Static_prop range_color in
      quant' sim_binding_color blk_color

    method build_All () = max_color

    method build_No () = max_color

    method build_Some_ () = max_color

    (* lbinop *)
    method build_LBin () f1' op' f2' = op' f1' f2'

    method build_And () = max_color

    method build_Iff () = max_color_wiwt

    method build_Imp () = max_color_wiwt

    method build_U () _ _ = Temporal

    method build_Or () = max_color_wiwt

    method build_R () _ _ = Temporal

    method build_S () _ _ = Temporal

    method build_T () _ _ = Temporal

    (* lunop *)
    method build_LUn () op' f' = op' f'

    method build_X () f' =
      match f' with Static_prop | Init -> Primed_prop | _ -> Temporal

    method build_F () _ = Temporal

    method build_G () f' =
      match f' with
      | Init | Static_prop | Invar ->
          Invar
      | Primed_prop | Trans ->
          Trans
      | _ ->
          Temporal

    method build_H () _ = Temporal

    method build_O () _ = Temporal

    method build_P () _ = Temporal

    method build_Not () f' = max_color_wiwt Static_prop f'

    (* compo_op *)
    method build_RComp () f1' op' f2' = op' f1' f2'

    method build_REq () = max_color_wiwt

    method build_In () = max_color_wiwt

    method build_NotIn () = max_color_wiwt

    method build_RNEq () = max_color_wiwt

    (* icomp_op *)
    method build_IComp () e1' op' e2' = op' e1' e2'

    method build_Gt () = max_color_wiwt

    method build_Gte () = max_color_wiwt

    method build_IEq () = max_color_wiwt

    method build_INEq () = max_color_wiwt

    method build_Lt () = max_color_wiwt

    method build_Lte () = max_color_wiwt

    (************************** exp  ********************************)
    method build_exp () pe' _ _ = pe'

    method build_oexp () color _ = color

    method build_Compr () sbs' b' =
      let blk_color = List.fold_left max_color_wiwt Static_prop b' in
      let max_color_for_simbindings
          (color_acc : goal_color)
          ((__disj1, __vars1, e1) : bool * int * goal_color) =
        max_color_wiwt color_acc e1
      in
      let sim_bindings_color =
        List.fold_left max_color_for_simbindings Static_prop sbs'
      in
      max_color_wiwt sim_bindings_color blk_color

    method build_Iden () = Static_prop

    method build_Name () r = if is_const elo r then Static_prop else Init

    method build_Var () __id = Static_prop

    method build_None_ () = Static_prop

    method build_Univ () = Static_prop

    method build_Prime () f' =
      match f' with Static_prop | Init -> Primed_prop | _ -> Temporal

    method build_RIte () f' t' e' =
      match (f', t', e') with
      | Static_prop, Static_prop, Static_prop ->
          Static_prop
      | (Init | Static_prop), (Init | Static_prop), (Init | Static_prop) ->
          Init
      | ( (Primed_prop | Init | Static_prop)
        , (Primed_prop | Init | Static_prop)
        , (Primed_prop | Init | Static_prop) ) ->
          Primed_prop
      | _, _, _ ->
          Temporal

    (* rbinop *)
    method build_RBin () f1' op' f2' = op' f1' f2'

    method build_Union () = max_color_wiwt

    method build_Inter () = max_color_wiwt

    method build_Join () = max_color_wiwt

    method build_LProj () = max_color_wiwt

    method build_Prod () = max_color_wiwt

    method build_RProj () = max_color_wiwt

    method build_Diff () = max_color_wiwt

    method build_Over () = max_color_wiwt

    (* runop *)
    method build_RUn () op' e' = op' e'

    method build_RTClos () = Fun.id

    method build_Transpose () = Fun.id

    method build_TClos () = Fun.id

    (*********************************** iexp **************************************)
    method build_iexp () iexp' _ = iexp'

    method build_IBin () i1' op' i2' = op' i1' i2'

    method build_IUn () op' i' = op' i'

    method build_Num () _ = Static_prop

    method build_Add () = max_color_wiwt

    method build_Neg () = max_color_wiwt Static_prop

    method build_Sub () = max_color_wiwt

    method build_Card () r' = r'
  end

(* Computes the color (Invar, Static_prop, Init or Temporal) of an
     elo formula *)
let color elo elo_fml = (new computer elo)#visit_fml () elo_fml
