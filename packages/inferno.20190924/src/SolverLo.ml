(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

open UnifierSig
open SolverSig

module Make
  (X : TEVAR)
  (S : STRUCTURE)
  (O : OUTPUT with type 'a structure = 'a S.structure)
= struct

(* -------------------------------------------------------------------------- *)

(* The type [tevar] of term variables is provided by [X]. *)

type tevar =
  X.tevar

module XMap =
  Map.Make(struct include X type t = tevar end)

(* The type [variable] of type variables is provided by the unifier [U], which
   we build. *)

module U =
  Unifier.Make(S)

type variable =
  U.variable

(* The type [ischeme] is provided by the generalization engine [G], which we
   build. *)

module G =
  Generalization.Make(S)(U)

type ischeme =
    G.scheme

(* -------------------------------------------------------------------------- *)

(* Creation of fresh variables. *)

let fresh t =
  U.fresh t G.no_rank

(* -------------------------------------------------------------------------- *)

(* The syntax of constraints is as follows. *)

(* This syntax is exposed to the user in the low-level interface [SolverLo],
   but not in the high-level interface [SolverHi]. So, it could be easily
   modified if desired. *)

type rawco =
  | CTrue
  | CConj of rawco * rawco
  | CEq of variable * variable
  | CExist of variable * rawco
  | CInstance of tevar * variable * variable list WriteOnceRef.t
  | CDef of tevar * variable * rawco
  | CLet of (tevar * variable * ischeme WriteOnceRef.t) list
        * rawco
        * rawco
        * variable list WriteOnceRef.t

(* -------------------------------------------------------------------------- *)

(* The non-recursive wrapper function [solve] is parameterized by the flag
   [rectypes], which indicates whether recursive types are permitted. It
   expects a constraint and solves it; that is, either it fails with an
   exception, or it succeeds and fills the write-once references that are
   embedded in the syntax of the constraint. *)

exception Unbound of tevar
exception Unify = U.Unify
exception Cycle = U.Cycle

let solve (rectypes : bool) (c : rawco) : unit =

  (* Initialize the generalization engine. It has mutable state, so [state]
     does not need to be an explicit parameter of the recursive function
     [solve]. *)

  let state = G.init() in

  (* The recursive function [solve] is parameterized with an environment
     that maps term variables to type schemes. *)

  let rec solve (env : ischeme XMap.t) (c : rawco) : unit =
    match c with
    | CTrue ->
        ()
    | CConj (c1, c2) ->
        solve env c1;
        solve env c2
    | CEq (v, w) ->
        U.unify v w
    | CExist (v, c) ->
        (* We assume that the variable [v] has been created fresh, so it
           is globally unique, it carries no structure, and its rank is
           [no_rank]. The combinator interface enforces this property. *)
        G.register state v;
        solve env c
    | CInstance (x, w, witnesses_hook) ->
        (* The environment provides a type scheme for [x]. *)
        let s = try XMap.find x env with Not_found -> raise (Unbound x) in
        (* Instantiating this type scheme yields a variable [v], which we unify with
           [w]. It also yields a list of witnesses, which we record, as they will be
           useful during the decoding phase. *)
        let witnesses, v = G.instantiate state s in
        WriteOnceRef.set witnesses_hook witnesses;
        U.unify v w
    | CDef (x, v, c) ->
        solve (XMap.add x (G.trivial v) env) c
    | CLet (xvss, c1, c2, generalizable_hook) ->
        (* Warn the generalization engine that we entering the left-hand side of
           a [let] construct. *)
        G.enter state;
        (* Register the variables [vs] with the generalization engine, just as if
           they were existentially bound in [c1]. This is what they are, basically,
           but they also serve as named entry points. *)
        let vs = List.map (fun (_, v, _) -> v) xvss in
        List.iter (G.register state) vs;
        (* Solve the constraint [c1]. *)
        solve env c1;
        (* Ask the generalization engine to perform an occurs check, to adjust the
           ranks of the type variables in the young generation (i.e., all of the
           type variables that were registered since the call to [G.enter] above),
           and to construct a list [ss] of type schemes for our entry points. The
           generalization engine also produces a list [generalizable] of the young
           variables that should be universally quantified here. *)
        let generalizable, ss = G.exit rectypes state vs in
        (* Fill the write-once reference [generalizable_hook]. *)
        WriteOnceRef.set generalizable_hook generalizable;
        (* Extend the environment [env] and fill the write-once references
           [scheme_hook]. *)
        let env =
          List.fold_left2 (fun env (x, _, scheme_hook) s ->
            WriteOnceRef.set scheme_hook s;
            XMap.add x s env
          ) env xvss ss
        in
        (* Proceed to solve [c2] in the extended environment. *)
        solve env c2

  in
  solve XMap.empty c

(* -------------------------------------------------------------------------- *)

(* Decoding types. *)

(* A variable is decoded to its unique integer identifier, which (via the
   function [O.variable]) is turned into an output type. *)

let decode_variable (x : variable) : O.tyvar =
  (* The following assertion ensures that the decoder is invoked only
     after the solver has been run. It would not really make sense to
     invoke the decoder before running the solver. That said, at the
     time of writing this comment, the API does not expose the decoder,
     so the client should have no way of violating this assertion. *)
  assert (U.rank x <> G.no_rank);
  U.id x

let decode_variable_as_type (x : variable) : O.ty =
  O.variable (decode_variable x)

(* A type decoder is a function that transforms a unifier variable into an
   output type. We choose to decode types in an eager manner; that is, we take
   care of invoking the decoder, so that the client never needs to perform this
   task. As a result, we do not even need to expose the decoder to the client
   (although we could, if desired). *)

type decoder =
  variable -> O.ty

(* The state of the cyclic decoder cannot persist. We must create a new
   cyclic decoder at every invocation, otherwise the [mu] binders could
   be incorrectly placed in the output. *)

let decode_cyclic : decoder =
  fun x ->
  U.new_cyclic_decoder
    decode_variable_as_type
    O.structure
    (fun x t -> O.mu (decode_variable x) t)
    x

(* Because [O.ty] is a nominal representation of types, a type is decoded
  in the same way, regardless of how many type binders we have entered.
  This makes it possible for the state of an (acyclic) decoder to persist
  between invocations. Thanks to this property, the type decoding process
  requires only linear time and space, regardless of how many calls to the
  decoder are performed. *)

(* The function [new_decoder] returns a new decoder. If [rectypes] is on,
   the cyclic decoding function, which does not have persistent state, is
   used. If [rectypes] is off, then a new acyclic decoder, with persistent
   state, is created and returned. *)

let new_decoder rectypes =
  let decode_acyclic : decoder =
    U.new_acyclic_decoder
      decode_variable_as_type
      O.structure
  in
  if rectypes then decode_cyclic else decode_acyclic

(* The function [decode_scheme] is parameterized by a type decoder, [decode]. *)

let decode_scheme decode (s : ischeme) : O.scheme =
  List.map decode_variable (G.quantifiers s),
  decode (G.body s)

end

