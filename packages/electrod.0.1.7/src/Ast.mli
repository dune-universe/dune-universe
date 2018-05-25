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

type var = private BVar of Var.t
val bound_var : Var.t -> var
val equal_var : var -> var -> bool
type ident = private Var of Var.t | Name of Name.t
val var_ident : Var.t -> ident
val name_ident : Name.t -> ident
val var_ident_of_bound_var : var -> ident
val equal_ident : ident -> ident -> bool
type goal = (var, ident) Gen_goal.t
type t = {
  file : string option;
  domain : Domain.t;
  instance : Instance.t;
  sym : Symmetry.t list;
  invariants : (var, ident) Gen_goal.fml list;
  goal : goal;
  atom_renaming : (Atom.t, Atom.t) CCList.Assoc.t;
  name_renaming : (Name.t, Name.t) CCList.Assoc.t;
}
val make :
  string option ->
  Domain.t ->
  Instance.t -> Symmetry.t list -> (var, ident) Gen_goal.fml list -> goal -> t
val pp_var : Format.formatter -> var -> unit
val pp_ident : Format.formatter -> ident -> unit
val pp_goal : Format.formatter -> (var, ident) Gen_goal.t -> unit
val pp_fml : (var, ident) Gen_goal.fml Fmtc.t
val pp_prim_fml : Format.formatter -> (var, ident) Gen_goal.prim_fml -> unit
val pp_exp : (var, ident) Gen_goal.exp Fmtc.t
val pp_prim_exp : Format.formatter -> (var, ident) Gen_goal.prim_exp -> unit
val pp_iexp : Format.formatter -> (var, ident) Gen_goal.iexp -> unit
val pp_prim_iexp : Format.formatter -> (var, ident) Gen_goal.prim_iexp -> unit
val pp_block : (var, ident) Libelectrod__Gen_goal.block Fmtc.t
val pp_sim_binding : (var, ident) Gen_goal.sim_binding Fmtc.t
val pp : Format.formatter -> t -> unit
val substitute :
  < visit_'i : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      ident -> ident;
    visit_'v : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      var -> var;
    visit_Add : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.ibinop;
    visit_All : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.quant;
    visit_And : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Block : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_fml;
    visit_BoxJoin : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp ->
      (var, ident) Gen_goal.exp list -> (var, ident) Gen_goal.prim_exp;
    visit_Card : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_iexp;
    visit_Compr : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.sim_binding list ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_exp;
    visit_Diff : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_F : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_FIte : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml ->
      (var, ident) Gen_goal.fml ->
      (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.prim_fml;
    visit_False : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_fml;
    visit_G : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_Gt : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op;
    visit_Gte : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op;
    visit_H : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_IBin : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.iexp ->
      Gen_goal.ibinop ->
      (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.prim_iexp;
    visit_IComp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.iexp ->
      Gen_goal.icomp_op ->
      (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.prim_fml;
    visit_IEq : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op;
    visit_INEq : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op;
    visit_IUn : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.iunop -> (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.prim_iexp;
    visit_Iden : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_exp;
    visit_Ident : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      ident -> (var, ident) Gen_goal.prim_exp;
    visit_Iff : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Imp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_In : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.comp_op;
    visit_Inter : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_Join : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_LBin : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml ->
      Gen_goal.lbinop -> (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.prim_fml;
    visit_LProj : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_LUn : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.lunop -> (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.prim_fml;
    visit_Let : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.binding list ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_fml;
    visit_Lone : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.quant;
    visit_Lt : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op;
    visit_Lte : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op;
    visit_Neg : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.iunop;
    visit_No : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.quant;
    visit_None_ : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_exp;
    visit_Not : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_NotIn : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.comp_op;
    visit_Num : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      int -> (var, ident) Gen_goal.prim_iexp;
    visit_O : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_One : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.quant;
    visit_Or : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Over : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_P : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_Prime : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_Prod : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_Qual : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rqualify ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_fml;
    visit_Quant : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.quant ->
      (var, ident) Gen_goal.sim_binding list ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_fml;
    visit_R : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_RBin : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp ->
      Gen_goal.rbinop -> (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_RComp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp ->
      Gen_goal.comp_op ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_fml;
    visit_REq : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.comp_op;
    visit_RIte : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml ->
      (var, ident) Gen_goal.exp ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_RLone : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rqualify;
    visit_RNEq : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.comp_op;
    visit_RNo : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rqualify;
    visit_ROne : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rqualify;
    visit_RProj : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_RSome : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rqualify;
    visit_RTClos : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.runop;
    visit_RUn : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.runop -> (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_Run : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.t;
    visit_S : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Some_ : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.quant;
    visit_Sub : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.ibinop;
    visit_TClos : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.runop;
    visit_Transpose : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.runop;
    visit_True : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_fml;
    visit_U : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Union : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop;
    visit_Univ : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_exp;
    visit_X : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t -> Gen_goal.lunop;
    visit_binding : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.binding -> var * (var, ident) Gen_goal.exp;
    visit_block : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Libelectrod__Gen_goal.block;
    visit_comp_op : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.comp_op -> Gen_goal.comp_op;
    visit_disj : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      bool -> bool;
    visit_exp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.exp;
    visit_fml : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.fml;
    visit_ibinop : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.ibinop -> Gen_goal.ibinop;
    visit_icomp_op : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.icomp_op -> Gen_goal.icomp_op;
    visit_iexp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.iexp;
    visit_iunop : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.iunop -> Gen_goal.iunop;
    visit_lbinop : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.lbinop -> Gen_goal.lbinop;
    visit_lunop : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.lunop -> Gen_goal.lunop;
    visit_prim_exp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_exp -> (var, ident) Gen_goal.prim_exp;
    visit_prim_fml : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_fml -> (var, ident) Gen_goal.prim_fml;
    visit_prim_iexp : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_iexp -> (var, ident) Gen_goal.prim_iexp;
    visit_quant : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.quant -> Gen_goal.quant;
    visit_rbinop : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rbinop -> Gen_goal.rbinop;
    visit_rqualify : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.rqualify -> Gen_goal.rqualify;
    visit_runop : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      Gen_goal.runop -> Gen_goal.runop;
    visit_sim_binding : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.sim_binding ->
      bool * var list * (var, ident) Gen_goal.exp;
    visit_t : (Var.t, (var, ident) Gen_goal.prim_exp) CCList.Assoc.t ->
      (var, ident) Gen_goal.t -> (var, ident) Gen_goal.t >
val rename :
  < visit_'i : (Name.t, Name.t) CCList.Assoc.t -> ident -> ident;
    visit_'v : (Name.t, Name.t) CCList.Assoc.t -> var -> var;
    visit_Add : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.ibinop;
    visit_All : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.quant;
    visit_And : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Block : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_fml;
    visit_BoxJoin : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp ->
      (var, ident) Gen_goal.exp list -> (var, ident) Gen_goal.prim_exp;
    visit_Card : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_iexp;
    visit_Compr : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.sim_binding list ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_exp;
    visit_Diff : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_F : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_FIte : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml ->
      (var, ident) Gen_goal.fml ->
      (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.prim_fml;
    visit_False : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_fml;
    visit_G : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_Gt : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.icomp_op;
    visit_Gte : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.icomp_op;
    visit_H : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_IBin : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.iexp ->
      Gen_goal.ibinop ->
      (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.prim_iexp;
    visit_IComp : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.iexp ->
      Gen_goal.icomp_op ->
      (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.prim_fml;
    visit_IEq : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.icomp_op;
    visit_INEq : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.icomp_op;
    visit_IUn : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.iunop -> (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.prim_iexp;
    visit_Iden : (Name.t, Name.t) CCList.Assoc.t -> (var, ident) Gen_goal.prim_exp;
    visit_Ident : (Name.t, Name.t) CCList.Assoc.t ->
      ident -> (var, ident) Gen_goal.prim_exp;
    visit_Iff : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Imp : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_In : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.comp_op;
    visit_Inter : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_Join : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_LBin : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml ->
      Gen_goal.lbinop -> (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.prim_fml;
    visit_LProj : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_LUn : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.lunop -> (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.prim_fml;
    visit_Let : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.binding list ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_fml;
    visit_Lone : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.quant;
    visit_Lt : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.icomp_op;
    visit_Lte : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.icomp_op;
    visit_Neg : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.iunop;
    visit_No : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.quant;
    visit_None_ : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_exp;
    visit_Not : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_NotIn : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.comp_op;
    visit_Num : (Name.t, Name.t) CCList.Assoc.t ->
      int -> (var, ident) Gen_goal.prim_iexp;
    visit_O : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_One : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.quant;
    visit_Or : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Over : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_P : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_Prime : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_Prod : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_Qual : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.rqualify ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_fml;
    visit_Quant : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.quant ->
      (var, ident) Gen_goal.sim_binding list ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.prim_fml;
    visit_R : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_RBin : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp ->
      Gen_goal.rbinop -> (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_RComp : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp ->
      Gen_goal.comp_op ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_fml;
    visit_REq : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.comp_op;
    visit_RIte : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml ->
      (var, ident) Gen_goal.exp ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_RLone : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rqualify;
    visit_RNEq : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.comp_op;
    visit_RNo : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rqualify;
    visit_ROne : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rqualify;
    visit_RProj : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_RSome : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rqualify;
    visit_RTClos : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.runop;
    visit_RUn : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.runop -> (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.prim_exp;
    visit_Run : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Gen_goal.t;
    visit_S : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Some_ : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.quant;
    visit_Sub : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.ibinop;
    visit_TClos : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.runop;
    visit_Transpose : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.runop;
    visit_True : (Name.t, Name.t) CCList.Assoc.t -> (var, ident) Gen_goal.prim_fml;
    visit_U : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop;
    visit_Union : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop;
    visit_Univ : (Name.t, Name.t) CCList.Assoc.t -> (var, ident) Gen_goal.prim_exp;
    visit_X : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop;
    visit_binding : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.binding -> var * (var, ident) Gen_goal.exp;
    visit_block : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Libelectrod__Gen_goal.block ->
      (var, ident) Libelectrod__Gen_goal.block;
    visit_comp_op : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.comp_op -> Gen_goal.comp_op;
    visit_disj : (Name.t, Name.t) CCList.Assoc.t -> bool -> bool;
    visit_exp : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.exp -> (var, ident) Gen_goal.exp;
    visit_fml : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.fml -> (var, ident) Gen_goal.fml;
    visit_ibinop : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.ibinop -> Gen_goal.ibinop;
    visit_icomp_op : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.icomp_op -> Gen_goal.icomp_op;
    visit_iexp : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.iexp -> (var, ident) Gen_goal.iexp;
    visit_iunop : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.iunop -> Gen_goal.iunop;
    visit_lbinop : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lbinop -> Gen_goal.lbinop;
    visit_lunop : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.lunop -> Gen_goal.lunop;
    visit_prim_exp : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_exp -> (var, ident) Gen_goal.prim_exp;
    visit_prim_fml : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_fml -> (var, ident) Gen_goal.prim_fml;
    visit_prim_iexp : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.prim_iexp -> (var, ident) Gen_goal.prim_iexp;
    visit_quant : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.quant -> Gen_goal.quant;
    visit_rbinop : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.rbinop -> Gen_goal.rbinop;
    visit_rqualify : (Name.t, Name.t) CCList.Assoc.t ->
      Gen_goal.rqualify -> Gen_goal.rqualify;
    visit_runop : (Name.t, Name.t) CCList.Assoc.t -> Gen_goal.runop -> Gen_goal.runop;
    visit_sim_binding : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.sim_binding ->
      bool * var list * (var, ident) Gen_goal.exp;
    visit_t : (Name.t, Name.t) CCList.Assoc.t ->
      (var, ident) Gen_goal.t -> (var, ident) Gen_goal.t >
