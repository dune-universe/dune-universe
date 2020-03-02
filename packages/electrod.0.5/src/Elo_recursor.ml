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

[@@@warning "-27"]

open Elo

class virtual ['self] recursor =
  object (self : 'self)
    inherit [_] VisitorsRuntime.fold

    inherit [_] VisitorsRuntime.map

    method virtual build_Add : _

    method virtual build_All : _

    method virtual build_And : _

    method virtual build_Block : _

    method virtual build_Card : _

    method virtual build_Compr : _

    method virtual build_Diff : _

    method virtual build_F : _

    method virtual build_FIte : _

    method virtual build_False : _

    method virtual build_G : _

    method virtual build_Gt : _

    method virtual build_Gte : _

    method virtual build_H : _

    method virtual build_IBin : _

    method virtual build_IComp : _

    method virtual build_IEq : _

    method virtual build_INEq : _

    method virtual build_IUn : _

    method virtual build_Iden : _

    method virtual build_Iff : _

    method virtual build_Imp : _

    method virtual build_In : _

    method virtual build_Inter : _

    method virtual build_Join : _

    method virtual build_LBin : _

    method virtual build_LProj : _

    method virtual build_LUn : _

    method virtual build_Lt : _

    method virtual build_Lte : _

    method virtual build_Name : _

    method virtual build_Neg : _

    method virtual build_No : _

    method virtual build_None_ : _

    method virtual build_Not : _

    method virtual build_NotIn : _

    method virtual build_Num : _

    method virtual build_O : _

    method virtual build_Or : _

    method virtual build_Over : _

    method virtual build_P : _

    method virtual build_Prime : _

    method virtual build_Prod : _

    method virtual build_Quant : _

    method virtual build_R : _

    method virtual build_RBin : _

    method virtual build_RComp : _

    method virtual build_REq : _

    method virtual build_RIte : _

    method virtual build_RNEq : _

    method virtual build_RProj : _

    method virtual build_RTClos : _

    method virtual build_RUn : _

    method virtual build_S : _

    method virtual build_Some_ : _

    method virtual build_Sub : _

    method virtual build_TClos : _

    method virtual build_Transpose : _

    method virtual build_True : _

    method virtual build_U : _

    method virtual build_Union : _

    method virtual build_Univ : _

    method virtual build_Var : _

    method virtual build_X : _

    method virtual build_oexp : _

    method visit_exp env exp = self#visit_'exp env exp

    method visit_iexp env iexp = self#visit_'iexp env iexp

    method visit_fml env fml = self#visit_'fml env fml

    method visit_'exp env (Exp {node; _}) = self#visit_oexp env node

    method visit_'fml env (Fml {node; _}) = self#visit_ofml env node

    method visit_'iexp env (Iexp {node; _}) = self#visit_oiexp env node

    method visit_True env = self#build_True env

    method visit_False env = self#build_False env

    method visit_RComp env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'exp env _visitors_c0 in
      let _visitors_r1 = self#visit_comp_op env _visitors_c1 in
      let _visitors_r2 = self#visit_'exp env _visitors_c2 in
      self#build_RComp
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_IComp env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'iexp env _visitors_c0 in
      let _visitors_r1 = self#visit_icomp_op env _visitors_c1 in
      let _visitors_r2 = self#visit_'iexp env _visitors_c2 in
      self#build_IComp
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_LUn env _visitors_c0 _visitors_c1 =
      let _visitors_r0 = self#visit_lunop env _visitors_c0 in
      let _visitors_r1 = self#visit_'fml env _visitors_c1 in
      self#build_LUn env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method visit_LBin env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'fml env _visitors_c0 in
      let _visitors_r1 = self#visit_lbinop env _visitors_c1 in
      let _visitors_r2 = self#visit_'fml env _visitors_c2 in
      self#build_LBin
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_Quant env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_quant env _visitors_c0 in
      let _visitors_r1 =
        (fun (_visitors_c0, _visitors_c1, _visitors_c2) ->
          let _visitors_r0 =
            (fun _visitors_this -> _visitors_this) _visitors_c0
          in
          let _visitors_r1 =
            (fun _visitors_this -> _visitors_this) _visitors_c1
          in
          let _visitors_r2 = self#visit_'exp env _visitors_c2 in
          (_visitors_r0, _visitors_r1, _visitors_r2) )
          _visitors_c1
      in
      let _visitors_r2 = self#visit_list self#visit_'fml env _visitors_c2 in
      self#build_Quant
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_FIte env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'fml env _visitors_c0 in
      let _visitors_r1 = self#visit_'fml env _visitors_c1 in
      let _visitors_r2 = self#visit_'fml env _visitors_c2 in
      self#build_FIte
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_Block env _visitors_c0 =
      let _visitors_r0 = self#visit_list self#visit_'fml env _visitors_c0 in
      self#build_Block env _visitors_r0

    method visit_ofml env _visitors_this =
      match _visitors_this with
      | True ->
          self#visit_True env
      | False ->
          self#visit_False env
      | RComp (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_RComp env _visitors_c0 _visitors_c1 _visitors_c2
      | IComp (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_IComp env _visitors_c0 _visitors_c1 _visitors_c2
      | LUn (_visitors_c0, _visitors_c1) ->
          self#visit_LUn env _visitors_c0 _visitors_c1
      | LBin (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_LBin env _visitors_c0 _visitors_c1 _visitors_c2
      | Quant (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_Quant env _visitors_c0 _visitors_c1 _visitors_c2
      | FIte (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_FIte env _visitors_c0 _visitors_c1 _visitors_c2
      | Block _visitors_c0 ->
          self#visit_Block env _visitors_c0

    method visit_All env = self#build_All env

    method visit_Some_ env = self#build_Some_ env

    method visit_No env = self#build_No env

    method visit_quant env _visitors_this =
      match _visitors_this with
      | All ->
          self#visit_All env
      | Some_ ->
          self#visit_Some_ env
      | No ->
          self#visit_No env

    method visit_And env = self#build_And env

    method visit_Or env = self#build_Or env

    method visit_Imp env = self#build_Imp env

    method visit_Iff env = self#build_Iff env

    method visit_U env = self#build_U env

    method visit_R env = self#build_R env

    method visit_S env = self#build_S env

    method visit_lbinop env _visitors_this =
      match _visitors_this with
      | And ->
          self#visit_And env
      | Or ->
          self#visit_Or env
      | Imp ->
          self#visit_Imp env
      | Iff ->
          self#visit_Iff env
      | U ->
          self#visit_U env
      | R ->
          self#visit_R env
      | S ->
          self#visit_S env

    method visit_F env = self#build_F env

    method visit_G env = self#build_G env

    method visit_Not env = self#build_Not env

    method visit_O env = self#build_O env

    method visit_X env = self#build_X env

    method visit_H env = self#build_H env

    method visit_P env = self#build_P env

    method visit_lunop env _visitors_this =
      match _visitors_this with
      | F ->
          self#visit_F env
      | G ->
          self#visit_G env
      | Not ->
          self#visit_Not env
      | O ->
          self#visit_O env
      | X ->
          self#visit_X env
      | H ->
          self#visit_H env
      | P ->
          self#visit_P env

    method visit_In env = self#build_In env

    method visit_NotIn env = self#build_NotIn env

    method visit_REq env = self#build_REq env

    method visit_RNEq env = self#build_RNEq env

    method visit_comp_op env _visitors_this =
      match _visitors_this with
      | In ->
          self#visit_In env
      | NotIn ->
          self#visit_NotIn env
      | REq ->
          self#visit_REq env
      | RNEq ->
          self#visit_RNEq env

    method visit_IEq env = self#build_IEq env

    method visit_INEq env = self#build_INEq env

    method visit_Lt env = self#build_Lt env

    method visit_Lte env = self#build_Lte env

    method visit_Gt env = self#build_Gt env

    method visit_Gte env = self#build_Gte env

    method visit_icomp_op env _visitors_this =
      match _visitors_this with
      | IEq ->
          self#visit_IEq env
      | INEq ->
          self#visit_INEq env
      | Lt ->
          self#visit_Lt env
      | Lte ->
          self#visit_Lte env
      | Gt ->
          self#visit_Gt env
      | Gte ->
          self#visit_Gte env

    method visit_oexp env _visitors_this =
      let _visitors_r0 = self#visit_prim_oexp env _visitors_this.prim_exp in
      let _visitors_r1 =
        (fun _visitors_this -> _visitors_this) _visitors_this.arity
      in
      self#build_oexp env _visitors_this _visitors_r0 _visitors_r1

    method visit_None_ env = self#build_None_ env

    method visit_Univ env = self#build_Univ env

    method visit_Iden env = self#build_Iden env

    method visit_Var env _visitors_c0 =
      let _visitors_r0 = (fun _visitors_this -> _visitors_this) _visitors_c0 in
      self#build_Var env _visitors_c0 _visitors_r0

    method visit_Name env _visitors_c0 =
      let _visitors_r0 = (fun _visitors_this -> _visitors_this) _visitors_c0 in
      self#build_Name env _visitors_c0 _visitors_r0

    method visit_RUn env _visitors_c0 _visitors_c1 =
      let _visitors_r0 = self#visit_runop env _visitors_c0 in
      let _visitors_r1 = self#visit_'exp env _visitors_c1 in
      self#build_RUn env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method visit_RBin env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'exp env _visitors_c0 in
      let _visitors_r1 = self#visit_rbinop env _visitors_c1 in
      let _visitors_r2 = self#visit_'exp env _visitors_c2 in
      self#build_RBin
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_RIte env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'fml env _visitors_c0 in
      let _visitors_r1 = self#visit_'exp env _visitors_c1 in
      let _visitors_r2 = self#visit_'exp env _visitors_c2 in
      self#build_RIte
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_Compr env _visitors_c0 _visitors_c1 =
      let _visitors_r0 =
        self#visit_list
          (fun env (_visitors_c0, _visitors_c1, _visitors_c2) ->
            let _visitors_r0 =
              (fun _visitors_this -> _visitors_this) _visitors_c0
            in
            let _visitors_r1 =
              (fun _visitors_this -> _visitors_this) _visitors_c1
            in
            let _visitors_r2 = self#visit_'exp env _visitors_c2 in
            (_visitors_r0, _visitors_r1, _visitors_r2) )
          env
          _visitors_c0
      in
      let _visitors_r1 = self#visit_list self#visit_'fml env _visitors_c1 in
      self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method visit_Prime env _visitors_c0 =
      let _visitors_r0 = self#visit_'exp env _visitors_c0 in
      self#build_Prime env _visitors_c0 _visitors_r0

    method visit_prim_oexp env _visitors_this =
      match _visitors_this with
      | None_ ->
          self#visit_None_ env
      | Univ ->
          self#visit_Univ env
      | Iden ->
          self#visit_Iden env
      | Var _visitors_c0 ->
          self#visit_Var env _visitors_c0
      | Name _visitors_c0 ->
          self#visit_Name env _visitors_c0
      | RUn (_visitors_c0, _visitors_c1) ->
          self#visit_RUn env _visitors_c0 _visitors_c1
      | RBin (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_RBin env _visitors_c0 _visitors_c1 _visitors_c2
      | RIte (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_RIte env _visitors_c0 _visitors_c1 _visitors_c2
      | Compr (_visitors_c0, _visitors_c1) ->
          self#visit_Compr env _visitors_c0 _visitors_c1
      | Prime _visitors_c0 ->
          self#visit_Prime env _visitors_c0

    method visit_Transpose env = self#build_Transpose env

    method visit_TClos env = self#build_TClos env

    method visit_RTClos env = self#build_RTClos env

    method visit_runop env _visitors_this =
      match _visitors_this with
      | Transpose ->
          self#visit_Transpose env
      | TClos ->
          self#visit_TClos env
      | RTClos ->
          self#visit_RTClos env

    method visit_Union env = self#build_Union env

    method visit_Inter env = self#build_Inter env

    method visit_Over env = self#build_Over env

    method visit_LProj env = self#build_LProj env

    method visit_RProj env = self#build_RProj env

    method visit_Prod env = self#build_Prod env

    method visit_Diff env = self#build_Diff env

    method visit_Join env = self#build_Join env

    method visit_rbinop env _visitors_this =
      match _visitors_this with
      | Union ->
          self#visit_Union env
      | Inter ->
          self#visit_Inter env
      | Over ->
          self#visit_Over env
      | LProj ->
          self#visit_LProj env
      | RProj ->
          self#visit_RProj env
      | Prod ->
          self#visit_Prod env
      | Diff ->
          self#visit_Diff env
      | Join ->
          self#visit_Join env

    method visit_Num env _visitors_c0 =
      let _visitors_r0 = (fun _visitors_this -> _visitors_this) _visitors_c0 in
      self#build_Num env _visitors_c0 _visitors_r0

    method visit_Card env _visitors_c0 =
      let _visitors_r0 = self#visit_'exp env _visitors_c0 in
      self#build_Card env _visitors_c0 _visitors_r0

    method visit_IUn env _visitors_c0 _visitors_c1 =
      let _visitors_r0 = self#visit_iunop env _visitors_c0 in
      let _visitors_r1 = self#visit_'iexp env _visitors_c1 in
      self#build_IUn env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method visit_IBin env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_'iexp env _visitors_c0 in
      let _visitors_r1 = self#visit_ibinop env _visitors_c1 in
      let _visitors_r2 = self#visit_'iexp env _visitors_c2 in
      self#build_IBin
        env
        _visitors_c0
        _visitors_c1
        _visitors_c2
        _visitors_r0
        _visitors_r1
        _visitors_r2

    method visit_oiexp env _visitors_this =
      match _visitors_this with
      | Num _visitors_c0 ->
          self#visit_Num env _visitors_c0
      | Card _visitors_c0 ->
          self#visit_Card env _visitors_c0
      | IUn (_visitors_c0, _visitors_c1) ->
          self#visit_IUn env _visitors_c0 _visitors_c1
      | IBin (_visitors_c0, _visitors_c1, _visitors_c2) ->
          self#visit_IBin env _visitors_c0 _visitors_c1 _visitors_c2

    method visit_Neg env = self#build_Neg env

    method visit_iunop env _visitors_this =
      match _visitors_this with Neg -> self#visit_Neg env

    method visit_Add env = self#build_Add env

    method visit_Sub env = self#build_Sub env

    method visit_ibinop env _visitors_this =
      match _visitors_this with
      | Add ->
          self#visit_Add env
      | Sub ->
          self#visit_Sub env
  end
