(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
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

(* 
Static_prop: a proposition that does not include any variable relation
nor any temporal operator.
Primed_prop: a propostion that may include variabale and constant
relations, without any temporal operator except un-nested X (next) or
prime.
Invar: proposition of the form always (phi) where phi does not include
any temporal operator (the color pf phi is Init or Static_prop).
Init: proposition without any temporal operator.
Trans: proposition of the form always (phi) where the color of phi is
Primed_prop.
Temporal: any other proposition.
 *)
type goal_color = Static_prop | Primed_prop | Invar | Init | Trans | Temporal

let to_string (gc : goal_color) =
  match gc with
  | Static_prop -> "Static_prop"
  | Primed_prop -> "Primed_prop"
  | Invar -> "Invar"
  | Init -> "Init"
  | Trans -> "Trans"
  | Temporal -> "Temporal"
let pp out gc =
  Fmtc.(pf out "%a" (styled `Yellow string) (to_string gc))

(* Computes the most general color between two colors *)
let max_color c1 c2 =
  match c1, c2 with 
  | Static_prop, Static_prop -> Static_prop
  | (Init | Static_prop) , (Init | Static_prop)  -> Init
  | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop)
    -> Primed_prop                                
  | (Invar | Static_prop) , (Invar | Static_prop) -> Invar
  | (Trans | Static_prop) , (Trans | Static_prop) -> Trans
  | _ , _ -> Temporal

(* same as max_color, but without Invar nor Trans *)               
let max_color_wiwt c1 c2 =
  match c1, c2 with
  | Static_prop, Static_prop -> Static_prop
  | (Init | Static_prop) , (Init | Static_prop)  -> Init
  | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop)
    -> Primed_prop 
  | _ , _ -> Temporal

(* removes the top level always operator in an invariant elo formula *)
let rec remove_always_to_invar f =
  let open GenGoal in
  let {prim_fml; fml_loc} = f in
  match prim_fml with
    | LUn (G, subfml) -> subfml
    | LBin (fml1, And, fml2) ->
        let fml1' = remove_always_to_invar fml1 in
        let fml2' = remove_always_to_invar fml2 in
        {prim_fml= lbinary fml1' And fml2'; fml_loc}
    | _ -> f

(* adds an always operator to an (invariant) elo formula if the
   outermost operator is not an always *)
let add_always_to_invar f =
  let open GenGoal in
  let {prim_fml; fml_loc} = f in
  match prim_fml with
    | LUn (G, _) -> f
    | _ -> {prim_fml = lunary G f; fml_loc}


class ['e] invarComputation = object (_ : 'self)
  inherit ['self] GenGoal.fold

  method visit_'v (__env : 'e) = Fun.id

  method visit_'i (__env : 'e) = Fun.id

  method build_fml (__env : 'e) f' _ = f' 

  method build_Run (__env : 'e) blk' =
    List.fold_left
      max_color_wiwt
      Static_prop
      blk'

  method build_Check (__env : 'e) blk' =
    List.fold_left
      max_color_wiwt
      Static_prop
      blk'


  method build_True (__env : 'e) = Static_prop

  method build_False (__env : 'e) = Static_prop

  method build_Block (__env : 'e) blk_colors =
    List.fold_left
      max_color
      Static_prop
      blk_colors

  method build_FIte (__env : 'e) f t e =
    match f, t, e with
    | Static_prop, Static_prop, Static_prop -> Static_prop
    | (Init | Static_prop) , (Init | Static_prop) , (Init | Static_prop)
      -> Init
    | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop) ,
      (Primed_prop | Init | Static_prop)
      -> Primed_prop
    | _ , _, _ -> Temporal

  method build_Let (__env : 'e) __bs' __block'= assert false (* SIMPLIFIED *)

  (* quant *)

  method build_Quant (__env : 'e) quant' sim_bindings_colors blk_colors =
    let blk_color =
      List.fold_left
        max_color_wiwt
        Static_prop
        blk_colors
    in
    let max_color_for_simbindings color_acc (__disj1, __vars1, e1)  =
      max_color_wiwt color_acc e1
    in      
    let sim_bindings_color =
      List.fold_left
        max_color_for_simbindings
        Static_prop
        sim_bindings_colors
    in
    quant' sim_bindings_color blk_color

  method build_One (__env : 'e) = max_color

  method build_Lone (__env : 'e) = max_color

  method build_All (__env : 'e) =  max_color

  method build_No (__env : 'e) =  max_color

  method build_Some_ (__env : 'e) =  max_color    

  (* lbinop *)      

  method build_LBin (__env : 'e) f1' op' f2' = op' f1' f2'

  method build_And (__env : 'e) = max_color

  method build_Iff (__env : 'e) = max_color_wiwt

  method build_Imp (__env : 'e)  = max_color_wiwt

  method build_U (__env : 'e) _ _ = Temporal

  method build_Or (__env : 'e)  = max_color_wiwt

  method build_R (__env : 'e) _ _ = Temporal

  method build_S (__env : 'e) _ _ = Temporal

  (* lunop *)                     

  method build_LUn (__env : 'e) op' f' =
    op' f'

  method build_X (__env : 'e) f' =
    match f' with
    | Static_prop | Init -> Primed_prop
    | _ -> Temporal

  method build_F (__env : 'e) _ = Temporal

  method build_G (__env : 'e) f' =
    match f' with
    | Init | Static_prop | Invar -> Invar
    | Primed_prop | Trans -> Trans 
    | _ -> Temporal

  method build_H (__env : 'e) _ = Temporal

  method build_O (__env : 'e) _ = Temporal

  method build_P (__env : 'e) _ = Temporal

  method build_Not (__env : 'e) f' =
    max_color_wiwt Static_prop f'

  (* compo_op *)

  method build_RComp (__env : 'e) f1' op' f2' =
    op' f1' f2'

  method build_REq (__env : 'e) = max_color_wiwt

  method build_In (__env : 'e)  = max_color_wiwt

  method build_NotIn (__env : 'e) = max_color_wiwt

  method build_RNEq (__env : 'e) = max_color_wiwt

  (* icomp_op *)

  method build_IComp (__env : 'e) e1' op' e2' =
    op' e1' e2'

  method build_Gt (__env : 'e) = max_color_wiwt

  method build_Gte (__env : 'e) = max_color_wiwt

  method build_IEq (__env : 'e) = max_color_wiwt

  method build_INEq (__env : 'e) = max_color_wiwt

  method build_Lt (__env : 'e) = max_color_wiwt

  method build_Lte (__env : 'e) = max_color_wiwt

  (* rqualify *)

  method build_Qual (__env : 'e) (__q' : bool) (__r' : goal_color) = assert false (* SIMPLIFIED *)

  method build_RLone (__env : 'e) = assert false (* SIMPLIFIED *)

  method build_RNo (__env : 'e) = assert false (* SIMPLIFIED *)

  method build_ROne (__env : 'e) = assert false (* SIMPLIFIED *)

  method build_RSome (__env : 'e) = assert false (* SIMPLIFIED *)

  (************************** exp  ********************************)

  method build_exp (__env : 'e) pe' _ _ = pe'

  method build_Compr (__env : 'e) sbs' b' =
    let blk_color =
      List.fold_left
        max_color_wiwt
        Static_prop
        b'
    in

    let max_color_for_simbindings (color_acc : goal_color)
          ((__disj1, __vars1, e1) : bool * Elo.var list * goal_color)  =
      max_color_wiwt color_acc e1
    in      
    let sim_bindings_color =
      List.fold_left
        max_color_for_simbindings
        Static_prop
        sbs'
    in

    max_color_wiwt sim_bindings_color blk_color

  method build_Iden (__env : 'e) = Static_prop

  method build_BoxJoin (__env : 'e) __call' __args' = (* SIMPLIFIED *)
    assert false

  method build_Ident (env : 'e) id =
    let open Elo in
    match id with
      | Var _ ->
          Static_prop
      | Name r ->
          if env#is_const r then
            Static_prop
          else
            Init

  method build_None_ (__env : 'e) = Static_prop

  method build_Univ (__env : 'e) = Static_prop

  method build_Prime (__env : 'e) f' =
    match f' with
    | Static_prop | Init -> Primed_prop
    | _ -> Temporal

  method build_RIte (__env : 'e) f' t' e' =
    match f', t', e' with
    | Static_prop, Static_prop, Static_prop -> Static_prop
    | (Init | Static_prop) , (Init | Static_prop) , (Init | Static_prop)
      -> Init
    | (Primed_prop | Init | Static_prop) , (Primed_prop | Init | Static_prop) ,
      (Primed_prop | Init | Static_prop)
      -> Primed_prop
    | _ , _, _ -> Temporal


  (* rbinop *)

  method build_RBin (__env : 'e) f1' op' f2' =
    op' f1' f2'

  method build_Union (__env : 'e)  = max_color_wiwt


  method build_Inter (__env : 'e) = max_color_wiwt

  method build_Join (__env : 'e) = max_color_wiwt

  method build_LProj (__env : 'e) = max_color_wiwt
  method build_Prod (__env : 'e) = max_color_wiwt

  method build_RProj (__env : 'e) = max_color_wiwt
  method build_Diff (__env : 'e) = max_color_wiwt
  method build_Over (__env : 'e) = max_color_wiwt                           
  (* runop *)

  method build_RUn (__env : 'e) op' e' = op' e'

  method build_RTClos (__env : 'e) = Fun.id

  method build_Transpose (__env : 'e) = Fun.id

  method build_TClos (__env : 'e) = Fun.id


  (*********************************** iexp **************************************)

  method build_iexp (__env : 'e) iexp' _ = iexp'

  method build_IBin (__env : 'e) i1' op' i2' = op' i1' i2'

  method build_IUn (__env : 'e) op' i' = op' i'

  method build_Num (__env : 'e) _ = Static_prop

  method build_Add (__env : 'e) = max_color_wiwt

  method build_Neg (__env : 'e) = max_color_wiwt Static_prop

  method build_Sub (__env : 'e) = max_color_wiwt

  method build_Card (__env : 'e) r' = r'
end
