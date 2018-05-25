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

(** Abstraction of solver-specific LTL and models wrt any concrete
    implementation in a given model-checker.  *)

(** Abstract type for atomic propositions of LTL.  *)
module type ATOMIC_PROPOSITION =
  sig
    type t
      
    val make : Domain.t -> Name.t -> Tuple.t -> t
      
    val compare : t -> t -> int

    (** compare atoms as strings  *)
    val compare_string : t -> t -> int
    val equal : t -> t -> bool
    val hash  : t -> int
      

    (** None if non-enumerable; otw Some ar with ar >= 0  *)
    val domain_arity : t -> int option

    (** Says whether the atomic proposition corresponds to a const or var relation *)
    val is_const : t -> bool

    (*% Says whether the atomic proposition is partial (= 'lone' enum)  *)
    val is_partial : t -> bool

    (** [split_string s] returns the name and tuple that produced this string, [None]
        in case no such pair has arrived *)
    val split_string : string -> (Name.t * Tuple.t) option
    val split : t -> (Name.t * Tuple.t) option
                                   
    val pp : t Fmtc.t
  end

(** Abstract type of LTL (contains pElo connectives as well as basic counting
    capabilities).  *)
module type LTL = sig
  module Atomic : ATOMIC_PROPOSITION
    
  type tcomp = 
    | Lte 
    | Lt
    | Gte
    | Gt
    | Eq 
    | Neq

  type t = private
    | Comp of tcomp * term * term
    | True
    | False
    | Atomic of Atomic.t
    | Not of t
    | And of t * t
    | Or of t * t
    | Imp of t * t
    | Iff of t * t
    | Xor of t * t
    | Ite of t * t * t
    | X of t
    | F of t
    | G of t
    | Y of t
    | O of t
    | H of t
    | U of t * t
    | R of t * t
    | S of t * t
    | T of t * t               

  and term = private
    | Num of int 
    | Plus of term * term
    | Minus of term * term
    | Neg of term 
    | Count of t list

  val true_ : t
  val false_ : t

  val atomic : Atomic.t -> t

  val not_ : t -> t

  val and_ : t -> t Lazy.t -> t
  val or_ : t -> t Lazy.t -> t
  val implies : t -> t Lazy.t -> t
  val xor : t -> t -> t
  val iff : t -> t -> t

  val conj : t list -> t
  val disj : t list -> t

  val wedge : range:('a Sequence.t) -> ('a -> t Lazy.t) -> t
  val vee : range:('a Sequence.t) -> ('a -> t Lazy.t) -> t

  val ifthenelse : t -> t -> t -> t

  val next : t -> t
  val always : t -> t
  val eventually : t -> t

  val yesterday : t -> t
  val once : t -> t
  val historically : t -> t

  val until : t -> t -> t
  val releases : t -> t -> t
  val since : t -> t -> t
  val trigerred : t -> t -> t

  val num : int -> term
  val plus : term -> term -> term
  val minus : term -> term -> term
  val neg : term -> term
  val count : t list -> term

  val comp : tcomp -> term -> term -> t
  val lt : tcomp
  val lte : tcomp
  val gt : tcomp
  val gte : tcomp
  val eq : tcomp
  val neq : tcomp

  module Infix : sig
    (* precedence: from strongest to weakest *)
    (* 1 *)
    val ( !! ) : t -> t 
    (* 2 *)
    val ( +|| ) : t -> t Lazy.t -> t
    val ( +&& ) : t -> t Lazy.t -> t
    (* 3 *)
    val ( @=> ) : t -> t Lazy.t -> t
    val ( @<=> ) : t -> t -> t
  end

  val pp : Format.formatter -> t -> unit

  val pp_gather_variables :
    ?next_is_X:bool -> Atomic.t Sequence.t ref -> Format.formatter -> t -> unit

end


(** Builds an LTL implementation out of an implementation of atomicic
    propositions. *)
module LTL_from_Atomic :
  functor (At : ATOMIC_PROPOSITION) -> LTL with module Atomic = At


type script_type =
  | Default of string
  | File of string

(* Abstract type for a complete model to be given to a solver.  *)
module type MODEL = sig
  type ltl

  type atomic

  type t = private {
    elo : Elo.t;   
    init : (string * ltl) Sequence.t; (* fst: string repr of Elo formula *)
    invariant : (string * ltl) Sequence.t; (* fst: string repr of Elo formula *)
    trans : (string * ltl) Sequence.t; (* fst: string repr of Elo formula *)
    property : string * ltl                (* fst: string repr of Elo formula *)
  }

  val make :
    elo:Elo.t
    -> init:(string * ltl) Sequence.t
    -> invariant:(string * ltl) Sequence.t
    -> trans:(string * ltl) Sequence.t 
    -> property:(string * ltl) -> t
    
  (** [analyze domain script filename model] runs the solver on [model]
      ([filename helps creating a temporary file name]): in case of [Error], the
      result contains the POSIX error code and the error string output by the
      solver. If [script] is [None], then a default command script is used;
      otherwise it contains the name of a script file. [elo] is the Electrod
      model (used to interpret back a resulting trace). 

      If [no_analysis] is set to true, then no analysis is done (but the files are
      still generated and may be kept) and the function returns [No_trace]!*)
  val analyze : 
    conversion_time:Mtime.span
    -> cmd:string 
    -> script:script_type
    -> keep_files:bool
    -> no_analysis:bool
    -> elo:Elo.t
    -> file:string -> t -> Outcome.t

  val pp : ?margin:int -> Format.formatter -> t -> unit

end
