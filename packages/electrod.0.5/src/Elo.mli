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

type ('fml, 'exp, 'iexp) ofml = private
  | True
  | False
  | RComp of 'exp * comp_op * 'exp
  | IComp of 'iexp * icomp_op * 'iexp
  | LUn of lunop * 'fml
  | LBin of 'fml * lbinop * 'fml
  | Quant of quant * (bool * int * 'exp) * 'fml list
  | FIte of 'fml * 'fml * 'fml
  | Block of 'fml list

and quant = private
  | All
  | Some_
  | No

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

and ('fml, 'exp, 'iexp) oexp =
  { prim_exp : ('fml, 'exp, 'iexp) prim_oexp
  ; arity : int }

and ('fml, 'exp, 'iexp) prim_oexp = private
  | None_
  | Univ
  | Iden
  | Var of int
  | Name of Name.t
  | RUn of runop * 'exp
  | RBin of 'exp * rbinop * 'exp
  | RIte of 'fml * 'exp * 'exp
  | Compr of (bool * int * 'exp) list * 'fml list
  | Prime of 'exp

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

and ('fml, 'exp, 'iexp) oiexp = private
  | Num of int
  | Card of 'exp
  | IUn of iunop * 'iexp
  | IBin of 'iexp * ibinop * 'iexp

and iunop = private Neg

and ibinop = private
  | Add
  | Sub

type goal = private Run of (fml list * bool option) [@@unboxed]

and fml = private Fml of (fml, exp, iexp) ofml Hashcons.hash_consed
[@@unboxed]

and prim_exp = (fml, exp, iexp) prim_oexp

and exp = private Exp of (fml, exp, iexp) oexp Hashcons.hash_consed
[@@unboxed]

and iexp = private Iexp of (fml, exp, iexp) oiexp Hashcons.hash_consed
[@@unboxed]

type t =
  { file : string option
  ; domain : Domain.t
  ; instance : Instance.t
  ; sym : Symmetry.t list
  ; invariants : fml list
  ; goal : goal
  ; atom_renaming : (Atom.t, Atom.t) CCList.Assoc.t
  ; name_renaming : (Name.t, Name.t) CCList.Assoc.t }

val make :
     string option
  -> Domain.t
  -> Instance.t
  -> Symmetry.t list
  -> fml list
  -> goal
  -> (Atom.t, Atom.t) CCList.Assoc.t
  -> (Name.t, Name.t) CCList.Assoc.t
  -> t

val arity : exp -> int

val run : fml list -> bool option -> goal

val true_ : fml

val false_ : fml

val rcomp : exp -> comp_op -> exp -> fml

val icomp : iexp -> icomp_op -> iexp -> fml

val lbinary : fml -> lbinop -> fml -> fml

val sim_binding : bool -> int -> exp -> bool * int * exp

val quant : quant -> bool * int * exp -> fml list -> fml

val lunary : lunop -> fml -> fml

val block : fml list -> fml

val fite : fml -> fml -> fml -> fml

val all : quant

val some : quant

val no_ : quant

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

val none : exp

val univ : exp

val iden : exp

val var : ar:int -> int -> exp

val name : ar:int -> Name.t -> exp

val runary : ar:int -> runop -> exp -> exp

val rbinary : ar:int -> exp -> rbinop -> exp -> exp

val rite : ar:int -> fml -> exp -> exp -> exp

val compr : ar:int -> (bool * int * exp) list -> fml list -> exp

val prime : ar:int -> exp -> exp

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

val num : int -> iexp

val card : exp -> iexp

val iunary : iunop -> iexp -> iexp

val ibinary : iexp -> ibinop -> iexp -> iexp

val neg : iunop

val add : ibinop

val sub : ibinop

val kwd_styled : 'a Fmtc.t -> 'a Fmtc.t

val pp_comp_op : Format.formatter -> comp_op -> unit

val pp_icomp_op : Format.formatter -> icomp_op -> unit

val pp_lunop : Format.formatter -> lunop -> unit

val pp_lbinop : Format.formatter -> lbinop -> unit

val pp_quant : Format.formatter -> quant -> unit

val pp_runop : Format.formatter -> runop -> unit

val pp_rbinop : Format.formatter -> rbinop -> unit

val pp_iunop : Format.formatter -> iunop -> unit

val pp_ibinop : Format.formatter -> ibinop -> unit

val pp_var : Format.formatter -> int -> unit

val pp_sim_binding : int -> Format.formatter -> bool * int * exp -> unit

val pp_sim_bindings :
  int -> Format.formatter -> (bool * int * exp) list -> unit

val pp_oblock : 'a -> ('a -> 'b Fmtc.t) -> Format.formatter -> 'b list -> unit

val pp_ofml :
     int
  -> (int -> 'a Fmtc.t)
  -> (int -> Format.formatter -> 'b -> unit)
  -> (int -> Format.formatter -> 'c -> unit)
  -> Format.formatter
  -> ('a, 'b, 'c) ofml
  -> unit

val pp_prim_oexp :
     int
  -> (int -> 'a Fmtc.t)
  -> (int -> Format.formatter -> 'b -> unit)
  -> Format.formatter
  -> ('a, 'b, 'c) prim_oexp
  -> unit

val pp_oiexp :
     'a
  -> ('a -> Format.formatter -> 'b -> unit)
  -> ('a -> Format.formatter -> 'c -> unit)
  -> Format.formatter
  -> ('env, 'b, 'c) oiexp
  -> unit

val pp_fml : int -> fml Fmtc.t

val pp_block : int -> Format.formatter -> fml list -> unit

val pp_iexp : int -> Format.formatter -> iexp -> unit

val pp_prim_exp : int -> Format.formatter -> (fml, exp, iexp) prim_oexp -> unit

val pp_exp : int -> Format.formatter -> exp -> unit

val pp_goal : Format.formatter -> goal -> unit

val pp : Format.formatter -> t -> unit

val pp_fml_stats : Format.formatter -> int -> unit
(** Prints the number of times every subformula is referenced 
    (if number > [inf]). *)

class ['c] map :
  object ('c)
    constraint
    'c = < visit_'exp : 'env -> exp -> exp
         ; visit_'fml : 'env -> fml -> fml
         ; visit_'iexp : 'env -> iexp -> iexp
         ; visit_Add : 'env -> ibinop
         ; visit_All : 'env -> quant
         ; visit_And : 'env -> lbinop
         ; visit_Block : 'env -> fml list -> (fml, exp, iexp) ofml
         ; visit_Card : 'env -> exp -> (fml, exp, iexp) oiexp
         ; visit_Compr :
                'env
             -> (bool * int * exp) list
             -> fml list
             -> (fml, exp, iexp) prim_oexp
         ; visit_Diff : 'env -> rbinop
         ; visit_F : 'env -> lunop
         ; visit_FIte : 'env -> fml -> fml -> fml -> (fml, exp, iexp) ofml
         ; visit_False : 'env -> (fml, exp, iexp) ofml
         ; visit_G : 'env -> lunop
         ; visit_Gt : 'env -> icomp_op
         ; visit_Gte : 'env -> icomp_op
         ; visit_H : 'env -> lunop
         ; visit_IBin :
             'env -> iexp -> ibinop -> iexp -> (fml, exp, iexp) oiexp
         ; visit_IComp :
             'env -> iexp -> icomp_op -> iexp -> (fml, exp, iexp) ofml
         ; visit_IEq : 'env -> icomp_op
         ; visit_INEq : 'env -> icomp_op
         ; visit_IUn : 'env -> iunop -> iexp -> (fml, exp, iexp) oiexp
         ; visit_Iden : 'env -> (fml, exp, iexp) prim_oexp
         ; visit_Iff : 'env -> lbinop
         ; visit_Imp : 'env -> lbinop
         ; visit_In : 'env -> comp_op
         ; visit_Inter : 'env -> rbinop
         ; visit_Join : 'env -> rbinop
         ; visit_LBin : 'env -> fml -> lbinop -> fml -> (fml, exp, iexp) ofml
         ; visit_LProj : 'env -> rbinop
         ; visit_LUn : 'env -> lunop -> fml -> (fml, exp, iexp) ofml
         ; visit_Lt : 'env -> icomp_op
         ; visit_Lte : 'env -> icomp_op
         ; visit_Name : 'env -> Name.t -> (fml, exp, iexp) prim_oexp
         ; visit_Neg : 'env -> iunop
         ; visit_No : 'env -> quant
         ; visit_None_ : 'env -> (fml, exp, iexp) prim_oexp
         ; visit_Not : 'env -> lunop
         ; visit_NotIn : 'env -> comp_op
         ; visit_Num : 'env -> int -> (fml, exp, iexp) oiexp
         ; visit_O : 'env -> lunop
         ; visit_Or : 'env -> lbinop
         ; visit_Over : 'env -> rbinop
         ; visit_P : 'env -> lunop
         ; visit_Prime : 'env -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_Prod : 'env -> rbinop
         ; visit_Quant :
                'env
             -> quant
             -> bool * int * exp
             -> fml list
             -> (fml, exp, iexp) ofml
         ; visit_R : 'env -> lbinop
         ; visit_RBin :
             'env -> exp -> rbinop -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_RComp : 'env -> exp -> comp_op -> exp -> (fml, exp, iexp) ofml
         ; visit_REq : 'env -> comp_op
         ; visit_RIte : 'env -> fml -> exp -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_RNEq : 'env -> comp_op
         ; visit_RProj : 'env -> rbinop
         ; visit_RTClos : 'env -> runop
         ; visit_RUn : 'env -> runop -> exp -> (fml, exp, iexp) prim_oexp
         ; visit_S : 'env -> lbinop
         ; visit_Some_ : 'env -> quant
         ; visit_Sub : 'env -> ibinop
         ; visit_TClos : 'env -> runop
         ; visit_Transpose : 'env -> runop
         ; visit_True : 'env -> (fml, exp, iexp) ofml
         ; visit_U : 'env -> lbinop
         ; visit_Union : 'env -> rbinop
         ; visit_Univ : 'env -> (fml, exp, iexp) prim_oexp
         ; visit_Var : 'env -> int -> (fml, exp, iexp) prim_oexp
         ; visit_X : 'env -> lunop
         ; visit_comp_op : 'env -> comp_op -> comp_op
         ; visit_exp : 'env -> exp -> exp
         ; visit_fml : 'env -> fml -> fml
         ; visit_ibinop : 'env -> ibinop -> ibinop
         ; visit_icomp_op : 'env -> icomp_op -> icomp_op
         ; visit_iexp : 'env -> iexp -> iexp
         ; visit_iunop : 'env -> iunop -> iunop
         ; visit_lbinop : 'env -> lbinop -> lbinop
         ; visit_lunop : 'env -> lunop -> lunop
         ; visit_oexp : 'env -> (fml, exp, iexp) oexp -> (fml, exp, iexp) oexp
         ; visit_ofml : 'env -> (fml, exp, iexp) ofml -> (fml, exp, iexp) ofml
         ; visit_oiexp :
             'env -> (fml, exp, iexp) oiexp -> (fml, exp, iexp) oiexp
         ; visit_prim_oexp :
             'env -> (fml, exp, iexp) prim_oexp -> (fml, exp, iexp) prim_oexp
         ; visit_quant : 'env -> quant -> quant
         ; visit_rbinop : 'env -> rbinop -> rbinop
         ; visit_runop : 'env -> runop -> runop
         ; .. >

    method visit_'exp : 'env -> exp -> exp

    method visit_'fml : 'env -> fml -> fml

    method visit_'iexp : 'env -> iexp -> iexp

    method visit_Add : 'env -> ibinop

    method visit_All : 'env -> quant

    method visit_And : 'env -> lbinop

    method visit_Block : 'env -> fml list -> (fml, exp, iexp) ofml

    method visit_Card : 'env -> exp -> (fml, exp, iexp) oiexp

    method visit_Compr :
      'env -> (bool * int * exp) list -> fml list -> (fml, exp, iexp) prim_oexp

    method visit_Diff : 'env -> rbinop

    method visit_F : 'env -> lunop

    method visit_FIte : 'env -> fml -> fml -> fml -> (fml, exp, iexp) ofml

    method visit_False : 'env -> (fml, exp, iexp) ofml

    method visit_G : 'env -> lunop

    method visit_Gt : 'env -> icomp_op

    method visit_Gte : 'env -> icomp_op

    method visit_H : 'env -> lunop

    method visit_IBin :
      'env -> iexp -> ibinop -> iexp -> (fml, exp, iexp) oiexp

    method visit_IComp :
      'env -> iexp -> icomp_op -> iexp -> (fml, exp, iexp) ofml

    method visit_IEq : 'env -> icomp_op

    method visit_INEq : 'env -> icomp_op

    method visit_IUn : 'env -> iunop -> iexp -> (fml, exp, iexp) oiexp

    method visit_Iden : 'env -> (fml, exp, iexp) prim_oexp

    method visit_Iff : 'env -> lbinop

    method visit_Imp : 'env -> lbinop

    method visit_In : 'env -> comp_op

    method visit_Inter : 'env -> rbinop

    method visit_Join : 'env -> rbinop

    method visit_LBin : 'env -> fml -> lbinop -> fml -> (fml, exp, iexp) ofml

    method visit_LProj : 'env -> rbinop

    method visit_LUn : 'env -> lunop -> fml -> (fml, exp, iexp) ofml

    method visit_Lt : 'env -> icomp_op

    method visit_Lte : 'env -> icomp_op

    method visit_Name : 'env -> Name.t -> (fml, exp, iexp) prim_oexp

    method visit_Neg : 'env -> iunop

    method visit_No : 'env -> quant

    method visit_None_ : 'env -> (fml, exp, iexp) prim_oexp

    method visit_Not : 'env -> lunop

    method visit_NotIn : 'env -> comp_op

    method visit_Num : 'env -> int -> (fml, exp, iexp) oiexp

    method visit_O : 'env -> lunop

    method visit_Or : 'env -> lbinop

    method visit_Over : 'env -> rbinop

    method visit_P : 'env -> lunop

    method visit_Prime : 'env -> exp -> (fml, exp, iexp) prim_oexp

    method visit_Prod : 'env -> rbinop

    method visit_Quant :
      'env -> quant -> bool * int * exp -> fml list -> (fml, exp, iexp) ofml

    method visit_R : 'env -> lbinop

    method visit_RBin :
      'env -> exp -> rbinop -> exp -> (fml, exp, iexp) prim_oexp

    method visit_RComp : 'env -> exp -> comp_op -> exp -> (fml, exp, iexp) ofml

    method visit_REq : 'env -> comp_op

    method visit_RIte : 'env -> fml -> exp -> exp -> (fml, exp, iexp) prim_oexp

    method visit_RNEq : 'env -> comp_op

    method visit_RProj : 'env -> rbinop

    method visit_RTClos : 'env -> runop

    method visit_RUn : 'env -> runop -> exp -> (fml, exp, iexp) prim_oexp

    method visit_S : 'env -> lbinop

    method visit_Some_ : 'env -> quant

    method visit_Sub : 'env -> ibinop

    method visit_TClos : 'env -> runop

    method visit_Transpose : 'env -> runop

    method visit_True : 'env -> (fml, exp, iexp) ofml

    method visit_U : 'env -> lbinop

    method visit_Union : 'env -> rbinop

    method visit_Univ : 'env -> (fml, exp, iexp) prim_oexp

    method visit_Var : 'env -> int -> (fml, exp, iexp) prim_oexp

    method visit_X : 'env -> lunop

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method private visit_bool : 'env. 'env -> bool -> bool

    method private visit_bytes : 'env. 'env -> bytes -> bytes

    method private visit_char : 'env. 'env -> char -> char

    method visit_comp_op : 'env -> comp_op -> comp_op

    method visit_exp : 'env -> exp -> exp

    method private visit_float : 'env. 'env -> float -> float

    method visit_fml : 'env -> fml -> fml

    method visit_ibinop : 'env -> ibinop -> ibinop

    method visit_icomp_op : 'env -> icomp_op -> icomp_op

    method visit_iexp : 'env -> iexp -> iexp

    method private visit_int : 'env. 'env -> int -> int

    method private visit_int32 : 'env. 'env -> int32 -> int32

    method private visit_int64 : 'env. 'env -> int64 -> int64

    method visit_iunop : 'env -> iunop -> iunop

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'env -> lbinop -> lbinop

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'env -> lunop -> lunop

    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

    method visit_oexp : 'env -> (fml, exp, iexp) oexp -> (fml, exp, iexp) oexp

    method visit_ofml : 'env -> (fml, exp, iexp) ofml -> (fml, exp, iexp) ofml

    method visit_oiexp :
      'env -> (fml, exp, iexp) oiexp -> (fml, exp, iexp) oiexp

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_oexp :
      'env -> (fml, exp, iexp) prim_oexp -> (fml, exp, iexp) prim_oexp

    method visit_quant : 'env -> quant -> quant

    method visit_rbinop : 'env -> rbinop -> rbinop

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.    ('env -> 'a -> 'b) -> ('env -> 'e -> 'f) -> 'env
      -> ('a, 'e) result -> ('b, 'f) result

    method visit_runop : 'env -> runop -> runop

    method private visit_string : 'env. 'env -> string -> string

    method private visit_unit : 'env. 'env -> unit -> unit
  end

class virtual ['c] fold :
  object ('c)
    constraint
    'c = < build_Add : 'env -> 'g
         ; build_All : 'env -> 'h
         ; build_And : 'env -> 'i
         ; build_Block : 'env -> 'j list -> 'j
         ; build_Card : 'env -> 'k -> 'l
         ; build_Compr : 'env -> (bool * int * 'k) list -> 'j list -> 'm
         ; build_Diff : 'env -> 'n
         ; build_F : 'env -> 'o
         ; build_FIte : 'env -> 'j -> 'j -> 'j -> 'j
         ; build_False : 'env -> 'j
         ; build_G : 'env -> 'o
         ; build_Gt : 'env -> 'p
         ; build_Gte : 'env -> 'p
         ; build_H : 'env -> 'o
         ; build_IBin : 'env -> 'l -> 'g -> 'l -> 'l
         ; build_IComp : 'env -> 'l -> 'p -> 'l -> 'j
         ; build_IEq : 'env -> 'p
         ; build_INEq : 'env -> 'p
         ; build_IUn : 'env -> 'q -> 'l -> 'l
         ; build_Iden : 'env -> 'm
         ; build_Iff : 'env -> 'i
         ; build_Imp : 'env -> 'i
         ; build_In : 'env -> 'r
         ; build_Inter : 'env -> 'n
         ; build_Join : 'env -> 'n
         ; build_LBin : 'env -> 'j -> 'i -> 'j -> 'j
         ; build_LProj : 'env -> 'n
         ; build_LUn : 'env -> 'o -> 'j -> 'j
         ; build_Lt : 'env -> 'p
         ; build_Lte : 'env -> 'p
         ; build_Name : 'env -> Name.t -> 'm
         ; build_Neg : 'env -> 'q
         ; build_No : 'env -> 'h
         ; build_None_ : 'env -> 'm
         ; build_Not : 'env -> 'o
         ; build_NotIn : 'env -> 'r
         ; build_Num : 'env -> int -> 'l
         ; build_O : 'env -> 'o
         ; build_Or : 'env -> 'i
         ; build_Over : 'env -> 'n
         ; build_P : 'env -> 'o
         ; build_Prime : 'env -> 'k -> 'm
         ; build_Prod : 'env -> 'n
         ; build_Quant : 'env -> 'h -> bool * int * 'k -> 'j list -> 'j
         ; build_R : 'env -> 'i
         ; build_RBin : 'env -> 'k -> 'n -> 'k -> 'm
         ; build_RComp : 'env -> 'k -> 'r -> 'k -> 'j
         ; build_REq : 'env -> 'r
         ; build_RIte : 'env -> 'j -> 'k -> 'k -> 'm
         ; build_RNEq : 'env -> 'r
         ; build_RProj : 'env -> 'n
         ; build_RTClos : 'env -> 's
         ; build_RUn : 'env -> 's -> 'k -> 'm
         ; build_S : 'env -> 'i
         ; build_Some_ : 'env -> 'h
         ; build_Sub : 'env -> 'g
         ; build_TClos : 'env -> 's
         ; build_Transpose : 'env -> 's
         ; build_True : 'env -> 'j
         ; build_U : 'env -> 'i
         ; build_Union : 'env -> 'n
         ; build_Univ : 'env -> 'm
         ; build_Var : 'env -> int -> 'm
         ; build_X : 'env -> 'o
         ; build_oexp : 'env -> 'm -> int -> 'k
         ; visit_'exp : 'env -> exp -> 'k
         ; visit_'fml : 'env -> fml -> 'j
         ; visit_'iexp : 'env -> iexp -> 'l
         ; visit_Add : 'env -> 'g
         ; visit_All : 'env -> 'h
         ; visit_And : 'env -> 'i
         ; visit_Block : 'env -> fml list -> 'j
         ; visit_Card : 'env -> exp -> 'l
         ; visit_Compr : 'env -> (bool * int * exp) list -> fml list -> 'm
         ; visit_Diff : 'env -> 'n
         ; visit_F : 'env -> 'o
         ; visit_FIte : 'env -> fml -> fml -> fml -> 'j
         ; visit_False : 'env -> 'j
         ; visit_G : 'env -> 'o
         ; visit_Gt : 'env -> 'p
         ; visit_Gte : 'env -> 'p
         ; visit_H : 'env -> 'o
         ; visit_IBin : 'env -> iexp -> ibinop -> iexp -> 'l
         ; visit_IComp : 'env -> iexp -> icomp_op -> iexp -> 'j
         ; visit_IEq : 'env -> 'p
         ; visit_INEq : 'env -> 'p
         ; visit_IUn : 'env -> iunop -> iexp -> 'l
         ; visit_Iden : 'env -> 'm
         ; visit_Iff : 'env -> 'i
         ; visit_Imp : 'env -> 'i
         ; visit_In : 'env -> 'r
         ; visit_Inter : 'env -> 'n
         ; visit_Join : 'env -> 'n
         ; visit_LBin : 'env -> fml -> lbinop -> fml -> 'j
         ; visit_LProj : 'env -> 'n
         ; visit_LUn : 'env -> lunop -> fml -> 'j
         ; visit_Lt : 'env -> 'p
         ; visit_Lte : 'env -> 'p
         ; visit_Name : 'env -> Name.t -> 'm
         ; visit_Neg : 'env -> 'q
         ; visit_No : 'env -> 'h
         ; visit_None_ : 'env -> 'm
         ; visit_Not : 'env -> 'o
         ; visit_NotIn : 'env -> 'r
         ; visit_Num : 'env -> int -> 'l
         ; visit_O : 'env -> 'o
         ; visit_Or : 'env -> 'i
         ; visit_Over : 'env -> 'n
         ; visit_P : 'env -> 'o
         ; visit_Prime : 'env -> exp -> 'm
         ; visit_Prod : 'env -> 'n
         ; visit_Quant : 'env -> quant -> bool * int * exp -> fml list -> 'j
         ; visit_R : 'env -> 'i
         ; visit_RBin : 'env -> exp -> rbinop -> exp -> 'm
         ; visit_RComp : 'env -> exp -> comp_op -> exp -> 'j
         ; visit_REq : 'env -> 'r
         ; visit_RIte : 'env -> fml -> exp -> exp -> 'm
         ; visit_RNEq : 'env -> 'r
         ; visit_RProj : 'env -> 'n
         ; visit_RTClos : 'env -> 's
         ; visit_RUn : 'env -> runop -> exp -> 'm
         ; visit_S : 'env -> 'i
         ; visit_Some_ : 'env -> 'h
         ; visit_Sub : 'env -> 'g
         ; visit_TClos : 'env -> 's
         ; visit_Transpose : 'env -> 's
         ; visit_True : 'env -> 'j
         ; visit_U : 'env -> 'i
         ; visit_Union : 'env -> 'n
         ; visit_Univ : 'env -> 'm
         ; visit_Var : 'env -> int -> 'm
         ; visit_X : 'env -> 'o
         ; visit_comp_op : 'env -> comp_op -> 'r
         ; visit_exp : 'env -> exp -> 'k
         ; visit_fml : 'env -> fml -> 'j
         ; visit_ibinop : 'env -> ibinop -> 'g
         ; visit_icomp_op : 'env -> icomp_op -> 'p
         ; visit_iexp : 'env -> iexp -> 'l
         ; visit_iunop : 'env -> iunop -> 'q
         ; visit_lbinop : 'env -> lbinop -> 'i
         ; visit_lunop : 'env -> lunop -> 'o
         ; visit_oexp : 'env -> (fml, exp, iexp) oexp -> 'k
         ; visit_ofml : 'env -> (fml, exp, iexp) ofml -> 'j
         ; visit_oiexp : 'env -> (fml, exp, iexp) oiexp -> 'l
         ; visit_prim_oexp : 'env -> (fml, exp, iexp) prim_oexp -> 'm
         ; visit_quant : 'env -> quant -> 'h
         ; visit_rbinop : 'env -> rbinop -> 'n
         ; visit_runop : 'env -> runop -> 's
         ; .. >

    method virtual build_Add : 'env -> 'g

    method virtual build_All : 'env -> 'h

    method virtual build_And : 'env -> 'i

    method virtual build_Block : 'env -> 'j list -> 'j

    method virtual build_Card : 'env -> 'k -> 'l

    method virtual build_Compr :
      'env -> (bool * int * 'k) list -> 'j list -> 'm

    method virtual build_Diff : 'env -> 'n

    method virtual build_F : 'env -> 'o

    method virtual build_FIte : 'env -> 'j -> 'j -> 'j -> 'j

    method virtual build_False : 'env -> 'j

    method virtual build_G : 'env -> 'o

    method virtual build_Gt : 'env -> 'p

    method virtual build_Gte : 'env -> 'p

    method virtual build_H : 'env -> 'o

    method virtual build_IBin : 'env -> 'l -> 'g -> 'l -> 'l

    method virtual build_IComp : 'env -> 'l -> 'p -> 'l -> 'j

    method virtual build_IEq : 'env -> 'p

    method virtual build_INEq : 'env -> 'p

    method virtual build_IUn : 'env -> 'q -> 'l -> 'l

    method virtual build_Iden : 'env -> 'm

    method virtual build_Iff : 'env -> 'i

    method virtual build_Imp : 'env -> 'i

    method virtual build_In : 'env -> 'r

    method virtual build_Inter : 'env -> 'n

    method virtual build_Join : 'env -> 'n

    method virtual build_LBin : 'env -> 'j -> 'i -> 'j -> 'j

    method virtual build_LProj : 'env -> 'n

    method virtual build_LUn : 'env -> 'o -> 'j -> 'j

    method virtual build_Lt : 'env -> 'p

    method virtual build_Lte : 'env -> 'p

    method virtual build_Name : 'env -> Name.t -> 'm

    method virtual build_Neg : 'env -> 'q

    method virtual build_No : 'env -> 'h

    method virtual build_None_ : 'env -> 'm

    method virtual build_Not : 'env -> 'o

    method virtual build_NotIn : 'env -> 'r

    method virtual build_Num : 'env -> int -> 'l

    method virtual build_O : 'env -> 'o

    method virtual build_Or : 'env -> 'i

    method virtual build_Over : 'env -> 'n

    method virtual build_P : 'env -> 'o

    method virtual build_Prime : 'env -> 'k -> 'm

    method virtual build_Prod : 'env -> 'n

    method virtual build_Quant : 'env -> 'h -> bool * int * 'k -> 'j list -> 'j

    method virtual build_R : 'env -> 'i

    method virtual build_RBin : 'env -> 'k -> 'n -> 'k -> 'm

    method virtual build_RComp : 'env -> 'k -> 'r -> 'k -> 'j

    method virtual build_REq : 'env -> 'r

    method virtual build_RIte : 'env -> 'j -> 'k -> 'k -> 'm

    method virtual build_RNEq : 'env -> 'r

    method virtual build_RProj : 'env -> 'n

    method virtual build_RTClos : 'env -> 's

    method virtual build_RUn : 'env -> 's -> 'k -> 'm

    method virtual build_S : 'env -> 'i

    method virtual build_Some_ : 'env -> 'h

    method virtual build_Sub : 'env -> 'g

    method virtual build_TClos : 'env -> 's

    method virtual build_Transpose : 'env -> 's

    method virtual build_True : 'env -> 'j

    method virtual build_U : 'env -> 'i

    method virtual build_Union : 'env -> 'n

    method virtual build_Univ : 'env -> 'm

    method virtual build_Var : 'env -> int -> 'm

    method virtual build_X : 'env -> 'o

    method virtual build_oexp : 'env -> 'm -> int -> 'k

    method visit_'exp : 'env -> exp -> 'k

    method visit_'fml : 'env -> fml -> 'j

    method visit_'iexp : 'env -> iexp -> 'l

    method visit_Add : 'env -> 'g

    method visit_All : 'env -> 'h

    method visit_And : 'env -> 'i

    method visit_Block : 'env -> fml list -> 'j

    method visit_Card : 'env -> exp -> 'l

    method visit_Compr : 'env -> (bool * int * exp) list -> fml list -> 'm

    method visit_Diff : 'env -> 'n

    method visit_F : 'env -> 'o

    method visit_FIte : 'env -> fml -> fml -> fml -> 'j

    method visit_False : 'env -> 'j

    method visit_G : 'env -> 'o

    method visit_Gt : 'env -> 'p

    method visit_Gte : 'env -> 'p

    method visit_H : 'env -> 'o

    method visit_IBin : 'env -> iexp -> ibinop -> iexp -> 'l

    method visit_IComp : 'env -> iexp -> icomp_op -> iexp -> 'j

    method visit_IEq : 'env -> 'p

    method visit_INEq : 'env -> 'p

    method visit_IUn : 'env -> iunop -> iexp -> 'l

    method visit_Iden : 'env -> 'm

    method visit_Iff : 'env -> 'i

    method visit_Imp : 'env -> 'i

    method visit_In : 'env -> 'r

    method visit_Inter : 'env -> 'n

    method visit_Join : 'env -> 'n

    method visit_LBin : 'env -> fml -> lbinop -> fml -> 'j

    method visit_LProj : 'env -> 'n

    method visit_LUn : 'env -> lunop -> fml -> 'j

    method visit_Lt : 'env -> 'p

    method visit_Lte : 'env -> 'p

    method visit_Name : 'env -> Name.t -> 'm

    method visit_Neg : 'env -> 'q

    method visit_No : 'env -> 'h

    method visit_None_ : 'env -> 'm

    method visit_Not : 'env -> 'o

    method visit_NotIn : 'env -> 'r

    method visit_Num : 'env -> int -> 'l

    method visit_O : 'env -> 'o

    method visit_Or : 'env -> 'i

    method visit_Over : 'env -> 'n

    method visit_P : 'env -> 'o

    method visit_Prime : 'env -> exp -> 'm

    method visit_Prod : 'env -> 'n

    method visit_Quant : 'env -> quant -> bool * int * exp -> fml list -> 'j

    method visit_R : 'env -> 'i

    method visit_RBin : 'env -> exp -> rbinop -> exp -> 'm

    method visit_RComp : 'env -> exp -> comp_op -> exp -> 'j

    method visit_REq : 'env -> 'r

    method visit_RIte : 'env -> fml -> exp -> exp -> 'm

    method visit_RNEq : 'env -> 'r

    method visit_RProj : 'env -> 'n

    method visit_RTClos : 'env -> 's

    method visit_RUn : 'env -> runop -> exp -> 'm

    method visit_S : 'env -> 'i

    method visit_Some_ : 'env -> 'h

    method visit_Sub : 'env -> 'g

    method visit_TClos : 'env -> 's

    method visit_Transpose : 'env -> 's

    method visit_True : 'env -> 'j

    method visit_U : 'env -> 'i

    method visit_Union : 'env -> 'n

    method visit_Univ : 'env -> 'm

    method visit_Var : 'env -> int -> 'm

    method visit_X : 'env -> 'o

    method private visit_array :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a array -> 'b array

    method private visit_bool : 'env. 'env -> bool -> bool

    method private visit_bytes : 'env. 'env -> bytes -> bytes

    method private visit_char : 'env. 'env -> char -> char

    method visit_comp_op : 'env -> comp_op -> 'r

    method visit_exp : 'env -> exp -> 'k

    method private visit_float : 'env. 'env -> float -> float

    method visit_fml : 'env -> fml -> 'j

    method visit_ibinop : 'env -> ibinop -> 'g

    method visit_icomp_op : 'env -> icomp_op -> 'p

    method visit_iexp : 'env -> iexp -> 'l

    method private visit_int : 'env. 'env -> int -> int

    method private visit_int32 : 'env. 'env -> int32 -> int32

    method private visit_int64 : 'env. 'env -> int64 -> int64

    method visit_iunop : 'env -> iunop -> 'q

    method private visit_lazy_t :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a lazy_t -> 'b lazy_t

    method visit_lbinop : 'env -> lbinop -> 'i

    method private visit_list :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a list -> 'b list

    method visit_lunop : 'env -> lunop -> 'o

    method private visit_nativeint : 'env. 'env -> nativeint -> nativeint

    method visit_oexp : 'env -> (fml, exp, iexp) oexp -> 'k

    method visit_ofml : 'env -> (fml, exp, iexp) ofml -> 'j

    method visit_oiexp : 'env -> (fml, exp, iexp) oiexp -> 'l

    method private visit_option :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a option -> 'b option

    method visit_prim_oexp : 'env -> (fml, exp, iexp) prim_oexp -> 'm

    method visit_quant : 'env -> quant -> 'h

    method visit_rbinop : 'env -> rbinop -> 'n

    method private visit_ref :
      'env 'a 'b. ('env -> 'a -> 'b) -> 'env -> 'a ref -> 'b ref

    method private visit_result :
      'env 'a 'b 'e 'f.    ('env -> 'a -> 'b) -> ('env -> 'e -> 'f) -> 'env
      -> ('a, 'e) result -> ('b, 'f) result

    method visit_runop : 'env -> runop -> 's

    method private visit_string : 'env. 'env -> string -> string

    method private visit_unit : 'env. 'env -> unit -> unit
  end
