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

(** Implements the type for concrete ([Raw]) and abstract ([Ast]) syntax trees, before inference of De Bruijn indices and simplification into [Elo] trees. *)

type ('v, 'i) t = private Run of (('v, 'i) block * bool option) [@@unboxed]

and ('v, 'i) fml =
  { prim_fml : ('v, 'i) prim_fml
  ; fml_loc : Location.t
  }

and ('v, 'i) prim_fml = private
  | True
  | False
  | Qual of rqualify * ('v, 'i) exp
  | RComp of ('v, 'i) exp * comp_op * ('v, 'i) exp
  | IComp of ('v, 'i) iexp * icomp_op * ('v, 'i) iexp
  | LUn of lunop * ('v, 'i) fml
  | LBin of ('v, 'i) fml * lbinop * ('v, 'i) fml
  | Quant of quant * ('v, 'i) sim_binding list * ('v, 'i) block
  | Let of ('v, 'i) binding list * ('v, 'i) block
  | FIte of ('v, 'i) fml * ('v, 'i) fml * ('v, 'i) fml
  | Block of ('v, 'i) block

and ('v, 'i) binding = 'v * ('v, 'i) exp

and ('v, 'i) sim_binding = disj * 'v list * ('v, 'i) exp

and disj = bool

and ('v, 'i) block = ('v, 'i) fml list

and quant = private
  | All
  | Some_
  | No
  | One
  | Lone

and lbinop = private
  | And
  | Or
  | Imp
  | Iff
  | U
  | R
  | S

and lunop = private
  | F
  | G
  | Not
  | O
  | X
  | H
  | P

and comp_op = private
  | In
  | NotIn
  | REq
  | RNEq

and icomp_op = private
  | IEq
  | INEq
  | Lt
  | Lte
  | Gt
  | Gte

and ('v, 'i) exp =
  { prim_exp : ('v, 'i) prim_exp
  ; exp_loc : Location.t
  ; arity : int option
  }

and ('v, 'i) prim_exp = private
  | None_
  | Univ
  | Iden
  | Ident of 'i
  | RUn of runop * ('v, 'i) exp
  | RBin of ('v, 'i) exp * rbinop * ('v, 'i) exp
  | RIte of ('v, 'i) fml * ('v, 'i) exp * ('v, 'i) exp
  | BoxJoin of ('v, 'i) exp * ('v, 'i) exp list
  | Compr of ('v, 'i) sim_binding list * ('v, 'i) block
  | Prime of ('v, 'i) exp

and rqualify = private
  | ROne
  | RLone
  | RSome
  | RNo

and runop = private
  | Transpose
  | TClos
  | RTClos

and rbinop = private
  | Union
  | Inter
  | Over
  | LProj
  | RProj
  | Prod
  | Diff
  | Join

and ('v, 'i) iexp =
  { prim_iexp : ('v, 'i) prim_iexp
  ; iexp_loc : Location.t
  }

and ('v, 'i) prim_iexp = private
  | Num of int
  | Card of ('v, 'i) exp
  | IUn of iunop * ('v, 'i) iexp
  | IBin of ('v, 'i) iexp * ibinop * ('v, 'i) iexp

and iunop = private Neg

and ibinop = private
  | Add
  | Sub

class virtual ['c] map :
  object ('c)
    constraint
    'c = < visit_'i : 'd -> 'g -> 'h
         ; visit_'v : 'd -> 'i -> 'j
         ; visit_Add : 'd -> ibinop
         ; visit_All : 'd -> quant
         ; visit_And : 'd -> lbinop
         ; visit_Block : 'd -> ('i, 'g) block -> ('j, 'h) prim_fml
         ; visit_BoxJoin :
             'd -> ('i, 'g) exp -> ('i, 'g) exp list -> ('j, 'h) prim_exp
         ; visit_Card : 'd -> ('i, 'g) exp -> ('j, 'h) prim_iexp
         ; visit_Compr :
                'd
             -> ('i, 'g) sim_binding list
             -> ('i, 'g) block
             -> ('j, 'h) prim_exp
         ; visit_Diff : 'd -> rbinop
         ; visit_F : 'd -> lunop
         ; visit_FIte :
                'd
             -> ('i, 'g) fml
             -> ('i, 'g) fml
             -> ('i, 'g) fml
             -> ('j, 'h) prim_fml
         ; visit_False : 'd -> ('j, 'h) prim_fml
         ; visit_G : 'd -> lunop
         ; visit_Gt : 'd -> icomp_op
         ; visit_Gte : 'd -> icomp_op
         ; visit_H : 'd -> lunop
         ; visit_IBin :
                'd
             -> ('i, 'g) iexp
             -> ibinop
             -> ('i, 'g) iexp
             -> ('j, 'h) prim_iexp
         ; visit_IComp :
                'd
             -> ('i, 'g) iexp
             -> icomp_op
             -> ('i, 'g) iexp
             -> ('j, 'h) prim_fml
         ; visit_IEq : 'd -> icomp_op
         ; visit_INEq : 'd -> icomp_op
         ; visit_IUn : 'd -> iunop -> ('i, 'g) iexp -> ('j, 'h) prim_iexp
         ; visit_Iden : 'd -> ('j, 'h) prim_exp
         ; visit_Ident : 'd -> 'g -> ('j, 'h) prim_exp
         ; visit_Iff : 'd -> lbinop
         ; visit_Imp : 'd -> lbinop
         ; visit_In : 'd -> comp_op
         ; visit_Inter : 'd -> rbinop
         ; visit_Join : 'd -> rbinop
         ; visit_LBin :
             'd -> ('i, 'g) fml -> lbinop -> ('i, 'g) fml -> ('j, 'h) prim_fml
         ; visit_LProj : 'd -> rbinop
         ; visit_LUn : 'd -> lunop -> ('i, 'g) fml -> ('j, 'h) prim_fml
         ; visit_Let :
             'd -> ('i, 'g) binding list -> ('i, 'g) block -> ('j, 'h) prim_fml
         ; visit_Lone : 'd -> quant
         ; visit_Lt : 'd -> icomp_op
         ; visit_Lte : 'd -> icomp_op
         ; visit_Neg : 'd -> iunop
         ; visit_No : 'd -> quant
         ; visit_None_ : 'd -> ('j, 'h) prim_exp
         ; visit_Not : 'd -> lunop
         ; visit_NotIn : 'd -> comp_op
         ; visit_Num : 'd -> int -> ('j, 'h) prim_iexp
         ; visit_O : 'd -> lunop
         ; visit_One : 'd -> quant
         ; visit_Or : 'd -> lbinop
         ; visit_Over : 'd -> rbinop
         ; visit_P : 'd -> lunop
         ; visit_Prime : 'd -> ('i, 'g) exp -> ('j, 'h) prim_exp
         ; visit_Prod : 'd -> rbinop
         ; visit_Qual : 'd -> rqualify -> ('i, 'g) exp -> ('j, 'h) prim_fml
         ; visit_Quant :
                'd
             -> quant
             -> ('i, 'g) sim_binding list
             -> ('i, 'g) block
             -> ('j, 'h) prim_fml
         ; visit_R : 'd -> lbinop
         ; visit_RBin :
             'd -> ('i, 'g) exp -> rbinop -> ('i, 'g) exp -> ('j, 'h) prim_exp
         ; visit_RComp :
             'd -> ('i, 'g) exp -> comp_op -> ('i, 'g) exp -> ('j, 'h) prim_fml
         ; visit_REq : 'd -> comp_op
         ; visit_RIte :
                'd
             -> ('i, 'g) fml
             -> ('i, 'g) exp
             -> ('i, 'g) exp
             -> ('j, 'h) prim_exp
         ; visit_RLone : 'd -> rqualify
         ; visit_RNEq : 'd -> comp_op
         ; visit_RNo : 'd -> rqualify
         ; visit_ROne : 'd -> rqualify
         ; visit_RProj : 'd -> rbinop
         ; visit_RSome : 'd -> rqualify
         ; visit_RTClos : 'd -> runop
         ; visit_RUn : 'd -> runop -> ('i, 'g) exp -> ('j, 'h) prim_exp
         ; visit_Run : 'd -> ('i, 'g) block * bool option -> ('j, 'h) t
         ; visit_S : 'd -> lbinop
         ; visit_Some_ : 'd -> quant
         ; visit_Sub : 'd -> ibinop
         ; visit_TClos : 'd -> runop
         ; visit_Transpose : 'd -> runop
         ; visit_True : 'd -> ('j, 'h) prim_fml
         ; visit_U : 'd -> lbinop
         ; visit_Union : 'd -> rbinop
         ; visit_Univ : 'd -> ('j, 'h) prim_exp
         ; visit_X : 'd -> lunop
         ; visit_binding : 'd -> ('i, 'g) binding -> 'j * ('j, 'h) exp
         ; visit_block : 'd -> ('i, 'g) block -> ('j, 'h) block
         ; visit_comp_op : 'd -> comp_op -> comp_op
         ; visit_disj : 'd -> disj -> disj
         ; visit_exp : 'd -> ('i, 'g) exp -> ('j, 'h) exp
         ; visit_fml : 'd -> ('i, 'g) fml -> ('j, 'h) fml
         ; visit_ibinop : 'd -> ibinop -> ibinop
         ; visit_icomp_op : 'd -> icomp_op -> icomp_op
         ; visit_iexp : 'd -> ('i, 'g) iexp -> ('j, 'h) iexp
         ; visit_iunop : 'd -> iunop -> iunop
         ; visit_lbinop : 'd -> lbinop -> lbinop
         ; visit_lunop : 'd -> lunop -> lunop
         ; visit_prim_exp : 'd -> ('i, 'g) prim_exp -> ('j, 'h) prim_exp
         ; visit_prim_fml : 'd -> ('i, 'g) prim_fml -> ('j, 'h) prim_fml
         ; visit_prim_iexp : 'd -> ('i, 'g) prim_iexp -> ('j, 'h) prim_iexp
         ; visit_quant : 'd -> quant -> quant
         ; visit_rbinop : 'd -> rbinop -> rbinop
         ; visit_rqualify : 'd -> rqualify -> rqualify
         ; visit_runop : 'd -> runop -> runop
         ; visit_sim_binding :
             'd -> ('i, 'g) sim_binding -> disj * 'j list * ('j, 'h) exp
         ; visit_t : 'd -> ('i, 'g) t -> ('j, 'h) t
         ; .. >

    method virtual visit_'i : 'd -> 'g -> 'h

    method virtual visit_'v : 'd -> 'i -> 'j

    method visit_Add : 'd -> ibinop

    method visit_All : 'd -> quant

    method visit_And : 'd -> lbinop

    method visit_Block : 'd -> ('i, 'g) block -> ('j, 'h) prim_fml

    method visit_BoxJoin :
      'd -> ('i, 'g) exp -> ('i, 'g) exp list -> ('j, 'h) prim_exp

    method visit_Card : 'd -> ('i, 'g) exp -> ('j, 'h) prim_iexp

    method visit_Compr :
      'd -> ('i, 'g) sim_binding list -> ('i, 'g) block -> ('j, 'h) prim_exp

    method visit_Diff : 'd -> rbinop

    method visit_F : 'd -> lunop

    method visit_FIte :
      'd -> ('i, 'g) fml -> ('i, 'g) fml -> ('i, 'g) fml -> ('j, 'h) prim_fml

    method visit_False : 'd -> ('j, 'h) prim_fml

    method visit_G : 'd -> lunop

    method visit_Gt : 'd -> icomp_op

    method visit_Gte : 'd -> icomp_op

    method visit_H : 'd -> lunop

    method visit_IBin :
      'd -> ('i, 'g) iexp -> ibinop -> ('i, 'g) iexp -> ('j, 'h) prim_iexp

    method visit_IComp :
      'd -> ('i, 'g) iexp -> icomp_op -> ('i, 'g) iexp -> ('j, 'h) prim_fml

    method visit_IEq : 'd -> icomp_op

    method visit_INEq : 'd -> icomp_op

    method visit_IUn : 'd -> iunop -> ('i, 'g) iexp -> ('j, 'h) prim_iexp

    method visit_Iden : 'd -> ('j, 'h) prim_exp

    method visit_Ident : 'd -> 'g -> ('j, 'h) prim_exp

    method visit_Iff : 'd -> lbinop

    method visit_Imp : 'd -> lbinop

    method visit_In : 'd -> comp_op

    method visit_Inter : 'd -> rbinop

    method visit_Join : 'd -> rbinop

    method visit_LBin :
      'd -> ('i, 'g) fml -> lbinop -> ('i, 'g) fml -> ('j, 'h) prim_fml

    method visit_LProj : 'd -> rbinop

    method visit_LUn : 'd -> lunop -> ('i, 'g) fml -> ('j, 'h) prim_fml

    method visit_Let :
      'd -> ('i, 'g) binding list -> ('i, 'g) block -> ('j, 'h) prim_fml

    method visit_Lone : 'd -> quant

    method visit_Lt : 'd -> icomp_op

    method visit_Lte : 'd -> icomp_op

    method visit_Neg : 'd -> iunop

    method visit_No : 'd -> quant

    method visit_None_ : 'd -> ('j, 'h) prim_exp

    method visit_Not : 'd -> lunop

    method visit_NotIn : 'd -> comp_op

    method visit_Num : 'd -> int -> ('j, 'h) prim_iexp

    method visit_O : 'd -> lunop

    method visit_One : 'd -> quant

    method visit_Or : 'd -> lbinop

    method visit_Over : 'd -> rbinop

    method visit_P : 'd -> lunop

    method visit_Prime : 'd -> ('i, 'g) exp -> ('j, 'h) prim_exp

    method visit_Prod : 'd -> rbinop

    method visit_Qual : 'd -> rqualify -> ('i, 'g) exp -> ('j, 'h) prim_fml

    method visit_Quant :
         'd
      -> quant
      -> ('i, 'g) sim_binding list
      -> ('i, 'g) block
      -> ('j, 'h) prim_fml

    method visit_R : 'd -> lbinop

    method visit_RBin :
      'd -> ('i, 'g) exp -> rbinop -> ('i, 'g) exp -> ('j, 'h) prim_exp

    method visit_RComp :
      'd -> ('i, 'g) exp -> comp_op -> ('i, 'g) exp -> ('j, 'h) prim_fml

    method visit_REq : 'd -> comp_op

    method visit_RIte :
      'd -> ('i, 'g) fml -> ('i, 'g) exp -> ('i, 'g) exp -> ('j, 'h) prim_exp

    method visit_RLone : 'd -> rqualify

    method visit_RNEq : 'd -> comp_op

    method visit_RNo : 'd -> rqualify

    method visit_ROne : 'd -> rqualify

    method visit_RProj : 'd -> rbinop

    method visit_RSome : 'd -> rqualify

    method visit_RTClos : 'd -> runop

    method visit_RUn : 'd -> runop -> ('i, 'g) exp -> ('j, 'h) prim_exp

    method visit_Run : 'd -> ('i, 'g) block * bool option -> ('j, 'h) t

    method visit_S : 'd -> lbinop

    method visit_Some_ : 'd -> quant

    method visit_Sub : 'd -> ibinop

    method visit_TClos : 'd -> runop

    method visit_Transpose : 'd -> runop

    method visit_True : 'd -> ('j, 'h) prim_fml

    method visit_U : 'd -> lbinop

    method visit_Union : 'd -> rbinop

    method visit_Univ : 'd -> ('j, 'h) prim_exp

    method visit_X : 'd -> lunop

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method visit_binding : 'd -> ('i, 'g) binding -> 'j * ('j, 'h) exp

    method visit_block : 'd -> ('i, 'g) block -> ('j, 'h) block

    method private visit_bool : 'env. 'env -> disj -> disj

    method private visit_bytes : 'env. 'env -> bytes -> bytes

    method private visit_char : 'env. 'env -> char -> char

    method visit_comp_op : 'd -> comp_op -> comp_op

    method visit_disj : 'd -> disj -> disj

    method visit_exp : 'd -> ('i, 'g) exp -> ('j, 'h) exp

    method private visit_float : 'env. 'env -> float -> float

    method visit_fml : 'd -> ('i, 'g) fml -> ('j, 'h) fml

    method visit_ibinop : 'd -> ibinop -> ibinop

    method visit_icomp_op : 'd -> icomp_op -> icomp_op

    method visit_iexp : 'd -> ('i, 'g) iexp -> ('j, 'h) iexp

    method private visit_int : 'env. 'env -> int -> int

    method private visit_int32 : 'env. 'env -> int32 -> int32

    method private visit_int64 : 'env. 'env -> int64 -> int64

    method visit_iunop : 'd -> iunop -> iunop

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'd -> lbinop -> lbinop

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'd -> lunop -> lunop

    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_exp : 'd -> ('i, 'g) prim_exp -> ('j, 'h) prim_exp

    method visit_prim_fml : 'd -> ('i, 'g) prim_fml -> ('j, 'h) prim_fml

    method visit_prim_iexp : 'd -> ('i, 'g) prim_iexp -> ('j, 'h) prim_iexp

    method visit_quant : 'd -> quant -> quant

    method visit_rbinop : 'd -> rbinop -> rbinop

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.    ('env -> 'a -> 'b) -> ('env -> 'e -> 'f) -> 'env
      -> ('a, 'e) result -> ('b, 'f) result

    method visit_rqualify : 'd -> rqualify -> rqualify

    method visit_runop : 'd -> runop -> runop

    method visit_sim_binding :
      'd -> ('i, 'g) sim_binding -> disj * 'j list * ('j, 'h) exp

    method private visit_string : 'env. 'env -> string -> string

    method visit_t : 'd -> ('i, 'g) t -> ('j, 'h) t

    method private visit_unit : 'env. 'env -> unit -> unit
  end

class virtual ['c] fold :
  object ('c)
    constraint
    'c = < build_Add : 'd -> 'g
         ; build_All : 'd -> 'h
         ; build_And : 'd -> 'i
         ; build_Block : 'd -> 'j list -> 'k
         ; build_BoxJoin : 'd -> 'l -> 'l list -> 'm
         ; build_Card : 'd -> 'l -> 'n
         ; build_Compr : 'd -> (disj * 'o list * 'l) list -> 'j list -> 'm
         ; build_Diff : 'd -> 'p
         ; build_F : 'd -> 'q
         ; build_FIte : 'd -> 'j -> 'j -> 'j -> 'k
         ; build_False : 'd -> 'k
         ; build_G : 'd -> 'q
         ; build_Gt : 'd -> 'r
         ; build_Gte : 'd -> 'r
         ; build_H : 'd -> 'q
         ; build_IBin : 'd -> 's -> 'g -> 's -> 'n
         ; build_IComp : 'd -> 's -> 'r -> 's -> 'k
         ; build_IEq : 'd -> 'r
         ; build_INEq : 'd -> 'r
         ; build_IUn : 'd -> 't -> 's -> 'n
         ; build_Iden : 'd -> 'm
         ; build_Ident : 'd -> 'u -> 'm
         ; build_Iff : 'd -> 'i
         ; build_Imp : 'd -> 'i
         ; build_In : 'd -> 'v
         ; build_Inter : 'd -> 'p
         ; build_Join : 'd -> 'p
         ; build_LBin : 'd -> 'j -> 'i -> 'j -> 'k
         ; build_LProj : 'd -> 'p
         ; build_LUn : 'd -> 'q -> 'j -> 'k
         ; build_Let : 'd -> ('o * 'l) list -> 'j list -> 'k
         ; build_Lone : 'd -> 'h
         ; build_Lt : 'd -> 'r
         ; build_Lte : 'd -> 'r
         ; build_Neg : 'd -> 't
         ; build_No : 'd -> 'h
         ; build_None_ : 'd -> 'm
         ; build_Not : 'd -> 'q
         ; build_NotIn : 'd -> 'v
         ; build_Num : 'd -> int -> 'n
         ; build_O : 'd -> 'q
         ; build_One : 'd -> 'h
         ; build_Or : 'd -> 'i
         ; build_Over : 'd -> 'p
         ; build_P : 'd -> 'q
         ; build_Prime : 'd -> 'l -> 'm
         ; build_Prod : 'd -> 'p
         ; build_Qual : 'd -> 'w -> 'l -> 'k
         ; build_Quant : 'd -> 'h -> (disj * 'o list * 'l) list -> 'j list -> 'k
         ; build_R : 'd -> 'i
         ; build_RBin : 'd -> 'l -> 'p -> 'l -> 'm
         ; build_RComp : 'd -> 'l -> 'v -> 'l -> 'k
         ; build_REq : 'd -> 'v
         ; build_RIte : 'd -> 'j -> 'l -> 'l -> 'm
         ; build_RLone : 'd -> 'w
         ; build_RNEq : 'd -> 'v
         ; build_RNo : 'd -> 'w
         ; build_ROne : 'd -> 'w
         ; build_RProj : 'd -> 'p
         ; build_RSome : 'd -> 'w
         ; build_RTClos : 'd -> 'x
         ; build_RUn : 'd -> 'x -> 'l -> 'm
         ; build_Run : 'd -> 'j list * bool option -> 'y
         ; build_S : 'd -> 'i
         ; build_Some_ : 'd -> 'h
         ; build_Sub : 'd -> 'g
         ; build_TClos : 'd -> 'x
         ; build_Transpose : 'd -> 'x
         ; build_True : 'd -> 'k
         ; build_U : 'd -> 'i
         ; build_Union : 'd -> 'p
         ; build_Univ : 'd -> 'm
         ; build_X : 'd -> 'q
         ; build_exp : 'd -> 'm -> Location.t -> int option -> 'l
         ; build_fml : 'd -> 'k -> Location.t -> 'j
         ; build_iexp : 'd -> 'n -> Location.t -> 's
         ; visit_'i : 'd -> 'z -> 'u
         ; visit_'v : 'd -> 'a1 -> 'o
         ; visit_Add : 'd -> 'g
         ; visit_All : 'd -> 'h
         ; visit_And : 'd -> 'i
         ; visit_Block : 'd -> ('a1, 'z) block -> 'k
         ; visit_BoxJoin : 'd -> ('a1, 'z) exp -> ('a1, 'z) exp list -> 'm
         ; visit_Card : 'd -> ('a1, 'z) exp -> 'n
         ; visit_Compr :
             'd -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'm
         ; visit_Diff : 'd -> 'p
         ; visit_F : 'd -> 'q
         ; visit_FIte :
             'd -> ('a1, 'z) fml -> ('a1, 'z) fml -> ('a1, 'z) fml -> 'k
         ; visit_False : 'd -> 'k
         ; visit_G : 'd -> 'q
         ; visit_Gt : 'd -> 'r
         ; visit_Gte : 'd -> 'r
         ; visit_H : 'd -> 'q
         ; visit_IBin : 'd -> ('a1, 'z) iexp -> ibinop -> ('a1, 'z) iexp -> 'n
         ; visit_IComp :
             'd -> ('a1, 'z) iexp -> icomp_op -> ('a1, 'z) iexp -> 'k
         ; visit_IEq : 'd -> 'r
         ; visit_INEq : 'd -> 'r
         ; visit_IUn : 'd -> iunop -> ('a1, 'z) iexp -> 'n
         ; visit_Iden : 'd -> 'm
         ; visit_Ident : 'd -> 'z -> 'm
         ; visit_Iff : 'd -> 'i
         ; visit_Imp : 'd -> 'i
         ; visit_In : 'd -> 'v
         ; visit_Inter : 'd -> 'p
         ; visit_Join : 'd -> 'p
         ; visit_LBin : 'd -> ('a1, 'z) fml -> lbinop -> ('a1, 'z) fml -> 'k
         ; visit_LProj : 'd -> 'p
         ; visit_LUn : 'd -> lunop -> ('a1, 'z) fml -> 'k
         ; visit_Let : 'd -> ('a1, 'z) binding list -> ('a1, 'z) block -> 'k
         ; visit_Lone : 'd -> 'h
         ; visit_Lt : 'd -> 'r
         ; visit_Lte : 'd -> 'r
         ; visit_Neg : 'd -> 't
         ; visit_No : 'd -> 'h
         ; visit_None_ : 'd -> 'm
         ; visit_Not : 'd -> 'q
         ; visit_NotIn : 'd -> 'v
         ; visit_Num : 'd -> int -> 'n
         ; visit_O : 'd -> 'q
         ; visit_One : 'd -> 'h
         ; visit_Or : 'd -> 'i
         ; visit_Over : 'd -> 'p
         ; visit_P : 'd -> 'q
         ; visit_Prime : 'd -> ('a1, 'z) exp -> 'm
         ; visit_Prod : 'd -> 'p
         ; visit_Qual : 'd -> rqualify -> ('a1, 'z) exp -> 'k
         ; visit_Quant :
             'd -> quant -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'k
         ; visit_R : 'd -> 'i
         ; visit_RBin : 'd -> ('a1, 'z) exp -> rbinop -> ('a1, 'z) exp -> 'm
         ; visit_RComp : 'd -> ('a1, 'z) exp -> comp_op -> ('a1, 'z) exp -> 'k
         ; visit_REq : 'd -> 'v
         ; visit_RIte :
             'd -> ('a1, 'z) fml -> ('a1, 'z) exp -> ('a1, 'z) exp -> 'm
         ; visit_RLone : 'd -> 'w
         ; visit_RNEq : 'd -> 'v
         ; visit_RNo : 'd -> 'w
         ; visit_ROne : 'd -> 'w
         ; visit_RProj : 'd -> 'p
         ; visit_RSome : 'd -> 'w
         ; visit_RTClos : 'd -> 'x
         ; visit_RUn : 'd -> runop -> ('a1, 'z) exp -> 'm
         ; visit_Run : 'd -> ('a1, 'z) block * bool option -> 'y
         ; visit_S : 'd -> 'i
         ; visit_Some_ : 'd -> 'h
         ; visit_Sub : 'd -> 'g
         ; visit_TClos : 'd -> 'x
         ; visit_Transpose : 'd -> 'x
         ; visit_True : 'd -> 'k
         ; visit_U : 'd -> 'i
         ; visit_Union : 'd -> 'p
         ; visit_Univ : 'd -> 'm
         ; visit_X : 'd -> 'q
         ; visit_binding : 'd -> ('a1, 'z) binding -> 'o * 'l
         ; visit_block : 'd -> ('a1, 'z) block -> 'j list
         ; visit_comp_op : 'd -> comp_op -> 'v
         ; visit_disj : 'd -> disj -> disj
         ; visit_exp : 'd -> ('a1, 'z) exp -> 'l
         ; visit_fml : 'd -> ('a1, 'z) fml -> 'j
         ; visit_ibinop : 'd -> ibinop -> 'g
         ; visit_icomp_op : 'd -> icomp_op -> 'r
         ; visit_iexp : 'd -> ('a1, 'z) iexp -> 's
         ; visit_iunop : 'd -> iunop -> 't
         ; visit_lbinop : 'd -> lbinop -> 'i
         ; visit_lunop : 'd -> lunop -> 'q
         ; visit_prim_exp : 'd -> ('a1, 'z) prim_exp -> 'm
         ; visit_prim_fml : 'd -> ('a1, 'z) prim_fml -> 'k
         ; visit_prim_iexp : 'd -> ('a1, 'z) prim_iexp -> 'n
         ; visit_quant : 'd -> quant -> 'h
         ; visit_rbinop : 'd -> rbinop -> 'p
         ; visit_rqualify : 'd -> rqualify -> 'w
         ; visit_runop : 'd -> runop -> 'x
         ; visit_sim_binding :
             'd -> ('a1, 'z) sim_binding -> disj * 'o list * 'l
         ; visit_t : 'd -> ('a1, 'z) t -> 'y
         ; .. >

    method virtual build_Add : 'd -> 'g

    method virtual build_All : 'd -> 'h

    method virtual build_And : 'd -> 'i

    method virtual build_Block : 'd -> 'j list -> 'k

    method virtual build_BoxJoin : 'd -> 'l -> 'l list -> 'm

    method virtual build_Card : 'd -> 'l -> 'n

    method virtual build_Compr :
      'd -> (disj * 'o list * 'l) list -> 'j list -> 'm

    method virtual build_Diff : 'd -> 'p

    method virtual build_F : 'd -> 'q

    method virtual build_FIte : 'd -> 'j -> 'j -> 'j -> 'k

    method virtual build_False : 'd -> 'k

    method virtual build_G : 'd -> 'q

    method virtual build_Gt : 'd -> 'r

    method virtual build_Gte : 'd -> 'r

    method virtual build_H : 'd -> 'q

    method virtual build_IBin : 'd -> 's -> 'g -> 's -> 'n

    method virtual build_IComp : 'd -> 's -> 'r -> 's -> 'k

    method virtual build_IEq : 'd -> 'r

    method virtual build_INEq : 'd -> 'r

    method virtual build_IUn : 'd -> 't -> 's -> 'n

    method virtual build_Iden : 'd -> 'm

    method virtual build_Ident : 'd -> 'u -> 'm

    method virtual build_Iff : 'd -> 'i

    method virtual build_Imp : 'd -> 'i

    method virtual build_In : 'd -> 'v

    method virtual build_Inter : 'd -> 'p

    method virtual build_Join : 'd -> 'p

    method virtual build_LBin : 'd -> 'j -> 'i -> 'j -> 'k

    method virtual build_LProj : 'd -> 'p

    method virtual build_LUn : 'd -> 'q -> 'j -> 'k

    method virtual build_Let : 'd -> ('o * 'l) list -> 'j list -> 'k

    method virtual build_Lone : 'd -> 'h

    method virtual build_Lt : 'd -> 'r

    method virtual build_Lte : 'd -> 'r

    method virtual build_Neg : 'd -> 't

    method virtual build_No : 'd -> 'h

    method virtual build_None_ : 'd -> 'm

    method virtual build_Not : 'd -> 'q

    method virtual build_NotIn : 'd -> 'v

    method virtual build_Num : 'd -> int -> 'n

    method virtual build_O : 'd -> 'q

    method virtual build_One : 'd -> 'h

    method virtual build_Or : 'd -> 'i

    method virtual build_Over : 'd -> 'p

    method virtual build_P : 'd -> 'q

    method virtual build_Prime : 'd -> 'l -> 'm

    method virtual build_Prod : 'd -> 'p

    method virtual build_Qual : 'd -> 'w -> 'l -> 'k

    method virtual build_Quant :
      'd -> 'h -> (disj * 'o list * 'l) list -> 'j list -> 'k

    method virtual build_R : 'd -> 'i

    method virtual build_RBin : 'd -> 'l -> 'p -> 'l -> 'm

    method virtual build_RComp : 'd -> 'l -> 'v -> 'l -> 'k

    method virtual build_REq : 'd -> 'v

    method virtual build_RIte : 'd -> 'j -> 'l -> 'l -> 'm

    method virtual build_RLone : 'd -> 'w

    method virtual build_RNEq : 'd -> 'v

    method virtual build_RNo : 'd -> 'w

    method virtual build_ROne : 'd -> 'w

    method virtual build_RProj : 'd -> 'p

    method virtual build_RSome : 'd -> 'w

    method virtual build_RTClos : 'd -> 'x

    method virtual build_RUn : 'd -> 'x -> 'l -> 'm

    method virtual build_Run : 'd -> 'j list * bool option -> 'y

    method virtual build_S : 'd -> 'i

    method virtual build_Some_ : 'd -> 'h

    method virtual build_Sub : 'd -> 'g

    method virtual build_TClos : 'd -> 'x

    method virtual build_Transpose : 'd -> 'x

    method virtual build_True : 'd -> 'k

    method virtual build_U : 'd -> 'i

    method virtual build_Union : 'd -> 'p

    method virtual build_Univ : 'd -> 'm

    method virtual build_X : 'd -> 'q

    method virtual build_exp : 'd -> 'm -> Location.t -> int option -> 'l

    method virtual build_fml : 'd -> 'k -> Location.t -> 'j

    method virtual build_iexp : 'd -> 'n -> Location.t -> 's

    method virtual visit_'i : 'd -> 'z -> 'u

    method virtual visit_'v : 'd -> 'a1 -> 'o

    method visit_Add : 'd -> 'g

    method visit_All : 'd -> 'h

    method visit_And : 'd -> 'i

    method visit_Block : 'd -> ('a1, 'z) block -> 'k

    method visit_BoxJoin : 'd -> ('a1, 'z) exp -> ('a1, 'z) exp list -> 'm

    method visit_Card : 'd -> ('a1, 'z) exp -> 'n

    method visit_Compr :
      'd -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'm

    method visit_Diff : 'd -> 'p

    method visit_F : 'd -> 'q

    method visit_FIte :
      'd -> ('a1, 'z) fml -> ('a1, 'z) fml -> ('a1, 'z) fml -> 'k

    method visit_False : 'd -> 'k

    method visit_G : 'd -> 'q

    method visit_Gt : 'd -> 'r

    method visit_Gte : 'd -> 'r

    method visit_H : 'd -> 'q

    method visit_IBin : 'd -> ('a1, 'z) iexp -> ibinop -> ('a1, 'z) iexp -> 'n

    method visit_IComp :
      'd -> ('a1, 'z) iexp -> icomp_op -> ('a1, 'z) iexp -> 'k

    method visit_IEq : 'd -> 'r

    method visit_INEq : 'd -> 'r

    method visit_IUn : 'd -> iunop -> ('a1, 'z) iexp -> 'n

    method visit_Iden : 'd -> 'm

    method visit_Ident : 'd -> 'z -> 'm

    method visit_Iff : 'd -> 'i

    method visit_Imp : 'd -> 'i

    method visit_In : 'd -> 'v

    method visit_Inter : 'd -> 'p

    method visit_Join : 'd -> 'p

    method visit_LBin : 'd -> ('a1, 'z) fml -> lbinop -> ('a1, 'z) fml -> 'k

    method visit_LProj : 'd -> 'p

    method visit_LUn : 'd -> lunop -> ('a1, 'z) fml -> 'k

    method visit_Let : 'd -> ('a1, 'z) binding list -> ('a1, 'z) block -> 'k

    method visit_Lone : 'd -> 'h

    method visit_Lt : 'd -> 'r

    method visit_Lte : 'd -> 'r

    method visit_Neg : 'd -> 't

    method visit_No : 'd -> 'h

    method visit_None_ : 'd -> 'm

    method visit_Not : 'd -> 'q

    method visit_NotIn : 'd -> 'v

    method visit_Num : 'd -> int -> 'n

    method visit_O : 'd -> 'q

    method visit_One : 'd -> 'h

    method visit_Or : 'd -> 'i

    method visit_Over : 'd -> 'p

    method visit_P : 'd -> 'q

    method visit_Prime : 'd -> ('a1, 'z) exp -> 'm

    method visit_Prod : 'd -> 'p

    method visit_Qual : 'd -> rqualify -> ('a1, 'z) exp -> 'k

    method visit_Quant :
      'd -> quant -> ('a1, 'z) sim_binding list -> ('a1, 'z) block -> 'k

    method visit_R : 'd -> 'i

    method visit_RBin : 'd -> ('a1, 'z) exp -> rbinop -> ('a1, 'z) exp -> 'm

    method visit_RComp : 'd -> ('a1, 'z) exp -> comp_op -> ('a1, 'z) exp -> 'k

    method visit_REq : 'd -> 'v

    method visit_RIte :
      'd -> ('a1, 'z) fml -> ('a1, 'z) exp -> ('a1, 'z) exp -> 'm

    method visit_RLone : 'd -> 'w

    method visit_RNEq : 'd -> 'v

    method visit_RNo : 'd -> 'w

    method visit_ROne : 'd -> 'w

    method visit_RProj : 'd -> 'p

    method visit_RSome : 'd -> 'w

    method visit_RTClos : 'd -> 'x

    method visit_RUn : 'd -> runop -> ('a1, 'z) exp -> 'm

    method visit_Run : 'd -> ('a1, 'z) block * bool option -> 'y

    method visit_S : 'd -> 'i

    method visit_Some_ : 'd -> 'h

    method visit_Sub : 'd -> 'g

    method visit_TClos : 'd -> 'x

    method visit_Transpose : 'd -> 'x

    method visit_True : 'd -> 'k

    method visit_U : 'd -> 'i

    method visit_Union : 'd -> 'p

    method visit_Univ : 'd -> 'm

    method visit_X : 'd -> 'q

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method visit_binding : 'd -> ('a1, 'z) binding -> 'o * 'l

    method visit_block : 'd -> ('a1, 'z) block -> 'j list

    method private visit_bool : 'env. 'env -> disj -> disj

    method private visit_bytes : 'env. 'env -> bytes -> bytes

    method private visit_char : 'env. 'env -> char -> char

    method visit_comp_op : 'd -> comp_op -> 'v

    method visit_disj : 'd -> disj -> disj

    method visit_exp : 'd -> ('a1, 'z) exp -> 'l

    method private visit_float : 'env. 'env -> float -> float

    method visit_fml : 'd -> ('a1, 'z) fml -> 'j

    method visit_ibinop : 'd -> ibinop -> 'g

    method visit_icomp_op : 'd -> icomp_op -> 'r

    method visit_iexp : 'd -> ('a1, 'z) iexp -> 's

    method private visit_int : 'env. 'env -> int -> int

    method private visit_int32 : 'env. 'env -> int32 -> int32

    method private visit_int64 : 'env. 'env -> int64 -> int64

    method visit_iunop : 'd -> iunop -> 't

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'd -> lbinop -> 'i

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'd -> lunop -> 'q

    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_exp : 'd -> ('a1, 'z) prim_exp -> 'm

    method visit_prim_fml : 'd -> ('a1, 'z) prim_fml -> 'k

    method visit_prim_iexp : 'd -> ('a1, 'z) prim_iexp -> 'n

    method visit_quant : 'd -> quant -> 'h

    method visit_rbinop : 'd -> rbinop -> 'p

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.    ('env -> 'a -> 'b) -> ('env -> 'e -> 'f) -> 'env
      -> ('a, 'e) result -> ('b, 'f) result

    method visit_rqualify : 'd -> rqualify -> 'w

    method visit_runop : 'd -> runop -> 'x

    method visit_sim_binding :
      'd -> ('a1, 'z) sim_binding -> disj * 'o list * 'l

    method private visit_string : 'env. 'env -> string -> string

    method visit_t : 'd -> ('a1, 'z) t -> 'y

    method private visit_unit : 'env. 'env -> unit -> unit
  end

val true_ : ('a, 'b) prim_fml

val false_ : ('a, 'b) prim_fml

val qual : rqualify -> ('a, 'b) exp -> ('a, 'b) prim_fml

val rcomp : ('a, 'b) exp -> comp_op -> ('a, 'b) exp -> ('a, 'b) prim_fml

val icomp : ('a, 'b) iexp -> icomp_op -> ('a, 'b) iexp -> ('a, 'b) prim_fml

val lbinary : ('a, 'b) fml -> lbinop -> ('a, 'b) fml -> ('a, 'b) prim_fml

val quant :
  quant -> ('a, 'b) sim_binding list -> ('a, 'b) block -> ('a, 'b) prim_fml

val lunary : lunop -> ('a, 'b) fml -> ('a, 'b) prim_fml

val block : ('a, 'b) block -> ('a, 'b) prim_fml

val fite : ('a, 'b) fml -> ('a, 'b) fml -> ('a, 'b) fml -> ('a, 'b) prim_fml

val let_ : ('a, 'b) binding list -> ('a, 'b) block -> ('a, 'b) prim_fml

val all : quant

val some : quant

val no_ : quant

val lone : quant

val one : quant

val and_ : lbinop

val or_ : lbinop

val impl : lbinop

val iff : lbinop

val until : lbinop

val releases : lbinop

val since : lbinop

val not_ : lunop

val sometime : lunop

val always : lunop

val once : lunop

val next : lunop

val historically : lunop

val previous : lunop

val num : int -> ('a, 'b) prim_iexp

val none : ('a, 'b) prim_exp

val univ : ('a, 'b) prim_exp

val iden : ('a, 'b) prim_exp

val ident : 'a -> ('b, 'a) prim_exp

val runary : runop -> ('a, 'b) exp -> ('a, 'b) prim_exp

val rbinary : ('a, 'b) exp -> rbinop -> ('a, 'b) exp -> ('a, 'b) prim_exp

val rite : ('a, 'b) fml -> ('a, 'b) exp -> ('a, 'b) exp -> ('a, 'b) prim_exp

val boxjoin : ('a, 'b) exp -> ('a, 'b) exp list -> ('a, 'b) prim_exp

val compr : ('a, 'b) sim_binding list -> ('a, 'b) block -> ('a, 'b) prim_exp

val prime : ('a, 'b) exp -> ('a, 'b) prim_exp

val in_ : comp_op

val not_in : comp_op

val req : comp_op

val rneq : comp_op

val ieq : icomp_op

val ineq : icomp_op

val lt : icomp_op

val lte : icomp_op

val gt : icomp_op

val gte : icomp_op

val rone : rqualify

val rsome : rqualify

val rlone : rqualify

val rno : rqualify

val transpose : runop

val tclos : runop

val rtclos : runop

val union : rbinop

val inter : rbinop

val over : rbinop

val lproj : rbinop

val rproj : rbinop

val prod : rbinop

val diff : rbinop

val join : rbinop

val card : ('a, 'b) exp -> ('a, 'b) prim_iexp

val iunary : iunop -> ('a, 'b) iexp -> ('a, 'b) prim_iexp

val ibinary : ('a, 'b) iexp -> ibinop -> ('a, 'b) iexp -> ('a, 'b) prim_iexp

val neg : iunop

val add : ibinop

val sub : ibinop

val fml : Location.t -> ('a, 'b) prim_fml -> ('a, 'b) fml

val exp : int option -> Location.t -> ('a, 'b) prim_exp -> ('a, 'b) exp

val iexp : Location.t -> ('a, 'b) prim_iexp -> ('a, 'b) iexp

val run : ('a, 'b) block -> bool option -> ('a, 'b) t

val get_expected : ('v, 'i) t -> bool option

val kwd_styled : 'a Fmtc.t -> 'a Fmtc.t

val pp :
     'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) t
  -> unit

val pp_fml :
  'a Fmtc.t -> (Format.formatter -> 'b -> unit) -> ('a, 'b) fml Fmtc.t

val pp_prim_fml :
     'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) prim_fml
  -> unit

val pp_block :
  'a Fmtc.t -> (Format.formatter -> 'b -> unit) -> ('a, 'b) block Fmtc.t

val pp_rqualify : Format.formatter -> rqualify -> unit

val pp_comp_op : Format.formatter -> comp_op -> unit

val pp_icomp_op : Format.formatter -> icomp_op -> unit

val pp_lunop : Format.formatter -> lunop -> unit

val pp_lbinop : Format.formatter -> lbinop -> unit

val pp_quant : Format.formatter -> quant -> unit

val pp_binding :
     sep:unit Fmtc.t
  -> 'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> ('a, 'b) binding Fmtc.t

val pp_sim_binding :
  'a Fmtc.t -> (Format.formatter -> 'b -> unit) -> ('a, 'b) sim_binding Fmtc.t

val pp_exp :
     ?show_arity:disj
  -> 'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> ('a, 'b) exp Fmtc.t

val pp_prim_exp :
     'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) prim_exp
  -> unit

val pp_runop : Format.formatter -> runop -> unit

val pp_rbinop : Format.formatter -> rbinop -> unit

val pp_iexp :
     'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) iexp
  -> unit

val pp_prim_iexp :
     'a Fmtc.t
  -> (Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b) prim_iexp
  -> unit

val pp_iunop : Format.formatter -> iunop -> unit

val pp_ibinop : Format.formatter -> ibinop -> unit
