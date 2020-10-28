(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Code

(* -------------------------------------------------------------------------- *)

(** When the combinator [nondet] is used, the reference implementation has
    access to a result of type ['c] produced by the candidate implementation.
    It must either accept the candidate's result and produce its own result of
    type ['r], or reject the candidate's result and produce a piece of OCaml
    code that explains why this result is unacceptable. This code is
    represented by a function of type [document -> document]. It receives the
    name of a variable, such as [observed], which stands for the candidate's
    result. This code could be an OCaml assertion that the observed result
    does not satisfy, or it could be just a comment. *)
type 'r diagnostic =
  | Valid of 'r
  | Invalid of (PPrint.document -> PPrint.document)

(** In the common case where ['r] and ['c] are the same type, the following
    type abbreviation is useful. The reference implementation must produce
    a result of type ['r nondet] instead of just ['r]. *)
type 'r nondet =
  'r -> 'r diagnostic

(* -------------------------------------------------------------------------- *)

(* A value of type [('r, 'c) spec] is a runtime representation of a
   specification. This specification describes an operation whose type in the
   reference implementation is ['r] and whose type in the candidate
   implementation is ['c]. *)

(* In other words, a specification can be thought of a binary (or
   relational) type, in contrast with the more common unary types
   that describe a single value. *)

(* -------------------------------------------------------------------------- *)

(* The data constructors of the type [(_, _) spec] can be organized in several
   groups:

   1. The data constructors for "positive" data, that is, data that can be
      constructed and deconstructed -- typically most data except functions.

   2. The data constructors for "negative" data, typically functions, which
      cannot be constructed or deconstructed, but can be used (applied).

   3. [SpecMapOutof] describes data that can be constructed, but not
      deconstructed or used in any way.

   4. [SpecIfPol] is eliminated by [normalize].

   5. [SpecDeferred] allows constructing recursive (cyclic) specifications. *)

type (_, _) spec =

  (* 1. Positive data. *)

  (* A constructible type is an OCaml type that is equipped with a generator.
     This must be a concrete type: the types ['r] and ['c] must coincide. *)
  | SpecConstructible :
      { generate : unit -> 't code } ->
      ('t, 't) spec

  (* An abstract base type has possibly different representations in the
     reference and candidate implementations. Its values are opaque (not
     observable). A tag allows testing at runtime whether two abstract base
     types are equal; this is used when one wishes to select a variable of
     appropriate type in the environment. *)

  (* We associate this tag with the product type ['r * c] so that the dynamic
     equality of two tags will imply a static type equality both on the
     reference side and on the implementation side. This is a situation where
     the injectivity of the product type is exploited: the equality of two
     product types implies the equality of their components. *)

  | SpecBaseAbstract:
      ('r * 'c) Tag.tag * ('r, 'c) abstract -> ('r, 'c) spec

  (* Unit. *)
  | SpecUnit:
      (unit, unit) spec

  (* Pairs. *)
  | SpecPair :
      ('r1, 'c1) spec * ('r2, 'c2) spec -> ('r1 * 'r2, 'c1 * 'c2) spec

  (* Options. *)
  | SpecOption :
      ('r, 'c) spec -> ('r option, 'c option) spec

  (* Results. *)
  | SpecResult :
      ('r1, 'c1) spec * ('r2, 'c2) spec ->
      (('r1, 'r2) result, ('c1, 'c2) result) spec

  (* Lists. *)
  | SpecList:
      int Gen.gen * ('r, 'c) spec ->
      ('r list, 'c list) spec

  (* A subset specification [Spec (spec, p)] restricts the specification
     [spec] to the subset of the values that satisfy the predicate [p].
     Technically, the predicate [p] applies to a reference-side value. This is
     again justified by the fact that we want to rely on the reference
     implementation when we determine which arguments are acceptable for an
     operation. *)
  (* This feature is typically used to require an argument of abstract type to
     satisfy a precondition. It can also be used at a concrete type, but this
     is not recommended, as one usually prefers to generate a suitable value
     directly, rather than first generate a possibly-unsuitable value and then
     eliminate it. *)
  | SpecSubset :
      ('r, 'c) spec * ('r -> bool) -> ('r, 'c) spec

  (* The mark [SpecNondet] is used to annotate the return type of an operation
     whose *specification* is nondeterministic. This indicates that several
     results are permitted; therefore, one cannot expect the reference
     implementation to produce "the" expected result. Instead, one must run
     the candidate implementation first, and give the reference implementation
     access to the result produced by the candidate. In many (albeit not all)
     situations, this is sufficient for the reference implementation to
     determine how it must behave. *)
  | SpecNondet :
      ('r, 'c) spec ->
      ('c -> 'r diagnostic, 'c) spec

  (* 2. Negative data. *)

  (* A deconstructible type is an OCaml type that is equipped with an equality
     test and a printer. This must be a concrete type: the types ['r] and ['c]
     must coincide. *)
  | SpecDeconstructible :
      { equal : ('t -> 't -> bool) code; print : 't -> PPrint.document } ->
      ('t, 't) spec

  (* [SpecTop] describes an output that must be ignored. *)
  | SpecTop :
      ('r, 'c) spec

  (* Arrows. *)
  | SpecArrow :
      ('r1, 'c1) spec * ('r2, 'c2) spec -> ('r1 -> 'r2, 'c1 -> 'c2) spec

  (* A dependent arrow allows the codomain to depend on a value of the domain.
     This allows naming an argument and referring to this name in the rest of
     the specification. Technically, the codomain depends on a value of type
     ['r1], the left projection of the domain. This is justified by the fact
     that we want to rely on the reference implementation when we determine
     which arguments are acceptable for an operation. *)
  | SpecDependentArrow :
      ('r1, 'c1) spec * ('r1 -> ('r2, 'c2) spec) -> ('r1 -> 'r2, 'c1 -> 'c2) spec

  (* [SpecMapInto] indicates that the user provides a value of type ['r1, 'c1]
     but wishes it to be transformed on the fly to a value of type ['r2, 'c2].
     To do so, the user provides a wrapper. Technically, the user must provide
     the name of this wrapper as well as its implementation on each side. The
     user also provides a description of the destination type, hence the name
     [SpecMapInto]. *)
  | SpecMapInto :
      ('r1 -> 'r2) *
      ('c1 -> 'c2) code *
      ('r2, 'c2) spec ->
      ('r1, 'c1) spec

  (* 3. Special case. *)

  (* [SpecMapOutof] indicates that the user provides a value of type ['r1, 'c1]
     but wishes it to be transformed on the fly to a value of type ['r2, 'c2].
     To do so, the user provides a wrapper. Technically, the user must provide
     the name of this wrapper as well as its implementation on each side. The
     user also provides a description of the source type, hence the name
     [SpecMapOutof]. *)
  | SpecMapOutof :
      ('r1 -> 'r2) *
      ('c1 -> 'c2) code *
      ('r1, 'c1) spec ->
      ('r2, 'c2) spec

  (* 4. Special cases. *)

  (* [SpecIfPol (neg, pos)] is a specification that is interpreted differently
     depending on whether it appears in a negative or positive position. *)
  | SpecIfPol :
      ('r, 'c) spec * ('r, 'c) spec ->
      ('r, 'c) spec

  (* [SpecDeferred] allows constructing recursive (cyclic) specifications. *)
  | SpecDeferred :
      ('r, 'c) spec Lazy.t ->
      ('r, 'c) spec

(* -------------------------------------------------------------------------- *)

(* The following information is associated with an abstract base type. *)

and ('r, 'c) abstract = {

  (* The base name used for a variable of this type. *)
  aty_var : string;

  (* A [check] function used to the check the well-formedness of a value
     of this type. *)
  aty_check : 'r -> ('c -> unit) code;

}

(* -------------------------------------------------------------------------- *)

(* Constructor functions. *)

(* Short public names for the constructors above. *)

let unit =
  SpecUnit

let ( *** ) first second =
  SpecPair (first, second)

let option spec =
  SpecOption spec

let result spec1 spec2 =
  SpecResult (spec1, spec2)

let list ?length:(length=Gen.lt 16) spec =
  SpecList (length, spec)

let ignored =
  SpecTop

let (^>) domain codomain =
  SpecArrow (domain, codomain)

let (^>>) domain codomain =
  SpecDependentArrow (domain, codomain)

let (%) p spec =
  SpecSubset (spec, p)

let nondet spec =
  SpecNondet spec

let map_into rwrap cwrap spec =
  SpecMapInto (rwrap, cwrap, spec)

let map_outof rwrap cwrap spec =
  SpecMapOutof (rwrap, cwrap, spec)

let ifpol neg pos =
  SpecIfPol (neg, pos)

let fix f =
  let rec spec = SpecDeferred (lazy (f spec)) in
  spec

let constructible (generate : unit -> 't code) =
  SpecConstructible { generate }

let easily_constructible (generate : unit -> 't) (print : 't -> PPrint.document) =
  let generate () =
    let value = generate() in
    value, Code.document (print value)
  in
  constructible generate

let deconstructible ?equal:(equal=((=), Code.infix "=")) print =
  SpecDeconstructible { equal; print }

(* -------------------------------------------------------------------------- *)

(* A value is a triple of a runtime type representation, a reference value,
   and a candidate value. *)

type value =
  | Value : ('r, 'c) spec * 'r * 'c -> value

(* -------------------------------------------------------------------------- *)

(* [declare_abstract_type] extends the type [spec] with a new case. The new
   type is regarded as abstract: its representation is [r] in the reference
   implementation and [c] in the candidate implementation. The new data
   constructor has type [(r, c) spec]. *)

let default_check : type r c . r -> (c -> unit) code =
  fun _ ->
    (fun _ -> ()), Code.constant "(fun _ -> ())"

let default_var =
  "x"

let declare_abstract_type
  ?check:(check=default_check)
  ?var:(var=default_var)
  ()
=
  (* Create a new tag for this abstract type. *)
  let tag = Tag.new_tag () in
  (* Done. *)
  SpecBaseAbstract (tag, { aty_var = var; aty_check = check })

(* -------------------------------------------------------------------------- *)

(* [normalize op polarity order_zero gen spec] checks that the specification
   [spec] is well-formed and normalizes it. *)

(* If the specification is ill-formed, the exception [IllFormedSpec] is
   raised. It is important to note that, because of [SpecDependentArrow], this
   exception is not necessarily raised when [normalize] is invoked; it can
   also be raised later on, when the normalized specification is used. *)

(* [op] is the name of the operation whose specification this is. *)

(* A positive [polarity] indicates an output; a negative polarity indicates an
   input. Because higher-order function types are not permitted, a positive
   polarity actually means that we are in a strictly positive position (that
   is, under zero arrows). Conversely, a negative polarity indicates that we
   are under exactly one arrow and therefore an order-0 type is expected. *)

(* The flag [order_zero] indicates that an order-0 type is expected, i.e.,
   arrows and dependent arrows are forbidden. Negative polarity implies
   [order_zero], but the converse is not true: order-0 is also required under
   a pair. *)

exception IllFormedSpec of (* operation, message: *) string * string

let ill_formed op format =
  Printf.ksprintf (fun s -> raise (IllFormedSpec (op, s))) format

let rec normalize
: type r c . string -> bool -> bool -> (r, c) spec -> (r, c) spec
= fun op polarity order_zero spec ->
  match spec with

  | SpecConstructible _ ->
      if polarity then
        ill_formed op
          "A constructible type cannot be used in a positive position.";
      spec

  | SpecDeconstructible _ ->
      spec (* TODO *)

  | SpecBaseAbstract _ ->
      spec

  | SpecUnit ->
      spec

  | SpecPair (first, second) ->
      (* Under a pair, we expect an order-0 type. *)
      let order_zero = true in
      let first = normalize op polarity order_zero first
      and second = normalize op polarity order_zero second in
      SpecPair (first, second)

  | SpecOption spec ->
      (* Under an option, we expect an order-0 type. *)
      let order_zero = true in
      let spec = normalize op polarity order_zero spec in
      SpecOption spec

  | SpecResult (spec1, spec2) ->
      let order_zero = true in
      let spec1 = normalize op polarity order_zero spec1
      and spec2 = normalize op polarity order_zero spec2 in
      SpecResult (spec1, spec2)

  | SpecList (n, spec) ->
      let order_zero = true in
      let spec = normalize op polarity order_zero spec in
      SpecList (n, spec)

  | SpecTop ->
      if not polarity then
        ill_formed op
          "The combinator `ignored` cannot be used in a negative position.";
      SpecTop

  | SpecArrow (domain, codomain) ->
      if order_zero then
        ill_formed op
          "The combinator `^>` cannot be used in the left-hand side\n\
           of a function or under a pair.";
      let domain = normalize op (not polarity) true domain
      and codomain = normalize op polarity order_zero codomain in
      SpecArrow (domain, codomain)

  | SpecDependentArrow (domain, codomain) ->
      if order_zero then
        ill_formed op
          "The combinator `^>>` cannot be used in the left-hand side\n\
           of a function or under a pair.";
      let domain = normalize op (not polarity) true domain
      (* Here, normalization is deferred until the moment where an actual
         argument is provided. Therefore, it is also repeated, every time
         an actual argument is provided. An alternative approach might be
         to generate an actual argument once, just for the purposes of
         normalizing and checking the codomain. However, not every type
         supports generation (in an empty environment); abstract types
         do not. *)
      and codomain rv = normalize op polarity order_zero (codomain rv) in
      SpecDependentArrow (domain, codomain)

  | SpecSubset (spec, p) ->
      if polarity then
        ill_formed op
          "The combinator `%%` cannot be used in a positive position.";
      let spec = normalize op polarity order_zero spec in
      SpecSubset (spec, p)

  | SpecNondet spec ->
      if not polarity then
        ill_formed op
          "The combinator `nondet` cannot be used in a negative position.";
      let spec = normalize op polarity order_zero spec in
      SpecNondet spec

  | SpecMapInto (rwrap, cwrap, spec) ->
      if not polarity then
        ill_formed op
          "The combinator `map_into` (%s) \
           cannot be used in a negative position."
          (Code.string cwrap);
      let spec = normalize op polarity order_zero spec in
      SpecMapInto (rwrap, cwrap, spec)

  | SpecMapOutof (rwrap, cwrap, spec) ->
      if polarity then
        ill_formed op
          "The combinator `map_outof` (%s) \
           cannot be used in a positive position."
          (Code.string cwrap);
      let spec = normalize op polarity order_zero spec in
      SpecMapOutof (rwrap, cwrap, spec)

  | SpecIfPol (neg, pos) ->
      normalize op polarity order_zero (if polarity then pos else neg)

  | SpecDeferred spec ->
      (* One might wish for [normalize] to eliminate [SpecDeferred]. However,
         doing this, while guaranteeing termination, would require some form
         of marking. For greater simplicity, we keep it and perform on-demand
         normalization. This means that we perform redundant normalization
         work as we iterate along a cycle. *)
      SpecDeferred (lazy (normalize op polarity order_zero (Lazy.force spec)))

let normalize op spec =
  let polarity = true
  and order_zero = false in
  normalize op polarity order_zero spec

let normalize_op (op, Value (spec, rv, cv)) =
  (op, Value (normalize op spec, rv, cv))
