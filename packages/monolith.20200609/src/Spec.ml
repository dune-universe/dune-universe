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
open Type

(* -------------------------------------------------------------------------- *)

(* A value of type [('r, 'c) spec] is a runtime representation of a
   specification. This specification describes an operation whose type in the
   reference implementation is ['r] and whose type in the candidate
   implementation is ['c]. *)

(* In other words, a specification can be thought of a binary (or relational)
   type, in constrast with the unary types defined whose runtime
   representation is defined in the module Type. *)

(* Specifications are richer than types in another dimension: they allow more
   precise descriptions of an operation's behavior, thanks to several
   constructs that do not exist in the syntax of types, such as the dependent
   arrow. *)

(* Two functions, [left] and [right], map a specification to its left and
   right projections, respectively. This gives us a (one-way) connection
   between binary specifications and unary types. Because specifications are
   richer than types, these functions are partial: e.g., the projections of a
   dependent arrow are not defined. *)

(* -------------------------------------------------------------------------- *)

(* The type [(_, _) spec] has the following constructors. *)

type (_, _) spec =

  (* A concrete base type has identical left and right projections, which is
     why the result type here is [('a, 'a) spec]. The values of a concrete
     base type are observable. A record of type ['a concrete] records some
     information about this concrete type. *)
  | SpecBaseConcrete :
      'a ty * 'a concrete -> ('a, 'a) spec

  (* An abstract base type has possibly different left and right projections,
     which is why the type variables ['r] and ['c] are distinct. The values
     of an abstract base type are opaque (not observable). A record of type
     [('r, 'c) abstract] records information about this abstract type. *)
  | SpecBaseAbstract:
      'r ty * 'c ty * ('r, 'c) abstract -> ('r, 'c) spec

  (* Pairs. *)
  | SpecPair :
      ('r1, 'c1) spec * ('r2, 'c2) spec -> ('r1 * 'r2, 'c1 * 'c2) spec

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
     results are permitted; therefore, it does not make sense to ask the
     reference implementation for "the" expected result and verify (via an
     equality test) that the candidate implementation produces the same
     result. Instead, one must run the candidate implementation first, and ask
     the reference implementation to verify (by whatever appropriate means)
     that this result is permitted by the specification. This explains why the
     reference implementation has type ['a postcondition] instead of ['a]. The
     type ['a postcondition] is defined in the module Type. For the moment,
     this feature is restricted to concrete base types. *)
  | SpecNondet :
      ('a, 'a) spec -> ('a postcondition, 'a) spec

  (* The mark [SpecMap] indicates that the user provides a value of type
     ['r1, 'c1] but wishes it to be transformed on the fly to a value of type
     ['r2, 'c2]. To do so, the user provides a constant [c]. (Technically,
     the user must provide the name of this constant and its implementation
     on each side.) *)
  | SpecMap :
      string * ('r1 -> 'r2) * ('c1 -> 'c2) *
      ('r1, 'c1) spec *
      ('r2, 'c2) spec ->
      ('r1, 'c1) spec

  (* [SpecGenerate] is used to equip a concrete type with a generator. This
     constructor can be eliminated by pushing the generator down into the
     [SpecBaseConcrete] node. We do so at the same time as we check for
     well-formedness check. This allows us to give a better error message
     when [SpecGenerate] is applied to something other than a concrete type. *)
  | SpecGenerate :
      ('t, 't) spec *
      (unit -> 't code) ->
      ('t, 't) spec
  | SpecGenerateSimplified :
      ('t, 't) spec *
      (unit -> 't) ->
      ('t, 't) spec

(* -------------------------------------------------------------------------- *)

(* The following information is associated with a concrete base type. *)

and 'a concrete = {

  (* The name of this type, used only in informational messages. *)
  cty_name : string;

  (* A printer for this type. *)
  cty_print : 'a -> PPrint.document;

  (* A generation function for this type. *)
  (* Optional. Required only if this type is used as an input. *)
  cty_generate : (unit -> 'a code) option;

  (* A notion of equality for this type. *)
  cty_equal : ('a -> 'a -> bool) code;

}

(* -------------------------------------------------------------------------- *)

(* The following information is associated with an abstract base type. *)

and ('r, 'c) abstract = {

  (* The name of this type, used only in informational messages. *)
  aty_name : string;

  (* The base name used for a variable of this type. *)
  aty_var : string;

  (* A [check] function used to the check the well-formedness of a value
     of this type. *)
  aty_check : 'r -> ('c -> unit) code;

}

(* -------------------------------------------------------------------------- *)

(* Constructor functions. *)

(* Short public names for the constructors above. *)

let ( *** ) first second =
  SpecPair (first, second)

let (^^>) domain codomain =
  SpecArrow (domain, codomain)

let (^&>) domain codomain =
  SpecDependentArrow (domain, codomain)

let (%) p spec =
  SpecSubset (spec, p)

let nondet spec =
  SpecNondet spec

let map name rc cc domain codomain =
  SpecMap (name, rc, cc, domain, codomain)

(* The following two constructors can be applied only to a concrete base
   type. They modify the generation function that is associated with this
   type. *)

(* In the first case, the user provides a function of type [unit -> r],
   and we use the default printer associated with this type to wrap it
   as a function of type [unit -> r code]. In the second case, the user
   provides a function of type [unit -> r code] directly. *)

let (&&@) spec generate =
  SpecGenerate (spec, generate)

let (&&&) spec generate =
  SpecGenerateSimplified (spec, generate)

(* -------------------------------------------------------------------------- *)

(* The left and right projections of a specification of type [('r, 'c) spec]
   have type ['r ty] and ['c ty], respectively. They are runtime
   representations of simple types. *)

(* The unary type [_ ty] does not have a dependent arrow constructor. (Even if
   it did, we would unable to define the right projection [right], because the
   codomain is parameterized on a value of the *left* projection.) Thus, both
   projections [left] and [right] are undefined at dependent arrows. We work
   around this problem by simplifying a dependent arrow on the fly into an
   ordinary arrow when we generate arguments for an application. *)


let rec left : type r c . (r, c) spec -> r ty
= function
  | SpecBaseConcrete (ty, _) ->
      ty
  | SpecBaseAbstract (ty, _, _) ->
      ty
  | SpecPair (first, second) ->
      TyPair (left first, left second)
  | SpecArrow (domain, codomain) ->
      TyArrow (left domain, left codomain)
  | SpecDependentArrow (_, _) ->
      (* [left] cannot be applied to a dependent arrow. *)
      assert false
  | SpecSubset (spec, _) ->
      left spec
  | SpecNondet spec ->
      TyNondet (left spec)
  | SpecMap (_, _, _, domain, _) ->
      left domain
  | SpecGenerate _ ->
      assert false
  | SpecGenerateSimplified _ ->
      assert false

let rec right : type r c . (r, c) spec -> c ty
= function
  | SpecBaseConcrete (ty, _) ->
      ty
  | SpecBaseAbstract (_, ty, _) ->
      ty
  | SpecPair (first, second) ->
      TyPair (right first, right second)
  | SpecArrow (domain, codomain) ->
      TyArrow (right domain, right codomain)
  | SpecDependentArrow (_, _) ->
      (* [right] cannot be applied to a dependent arrow. *)
      assert false
  | SpecSubset (spec, _) ->
      right spec
  | SpecNondet spec ->
      right spec
  | SpecMap (_, _, _, domain, _) ->
      right domain
  | SpecGenerate _ ->
      assert false
  | SpecGenerateSimplified _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* A value of type [rany] is a triple of a runtime type representation and
   two values. Intuitively, it represents an operation that has different
   implementations in the reference and the candidate. *)

type rany =
  | RAny : ('r, 'c) spec * 'r * 'c -> rany

(* -------------------------------------------------------------------------- *)

(* A value of type [xspec] is a value of type [('r, 'c) spec], for some types
   ['r] and ['c]. Therefore, like a value of type [('r, 'c) spec], it is just
   a runtime tag -- a runtime type representation. *)

type xspec =
  | XSpec : ('r, 'c) spec -> xspec [@@unboxed]

(* -------------------------------------------------------------------------- *)

(* Extracting the notion of equality associated with a concrete base type. *)

(* This equality has type [r -> c -> bool], but we re-package it at type
   [value -> value -> bool] because this happens to be more convenient. *)

(* TODO perhaps define directly the second version of [equal] *)

let equal (type r c) (spec : (r, c) spec) : (any -> any -> bool) code =
  match spec with
  | SpecBaseConcrete (ty, properties) ->
      Code.map (fun equal v1 v2 ->
        equal (unwrap ty v1) (unwrap ty v2)
      ) properties.cty_equal
  | _ ->
      assert false

let equal xspec : (any -> any -> bool) code =
  let XSpec spec = xspec in
  equal spec

(* -------------------------------------------------------------------------- *)

(* Extracting the printer for a concrete base type. *)

let print = function
  | XSpec (SpecBaseConcrete (ty, properties)) ->
      fun v -> properties.cty_print (unwrap ty v)
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* Extracting the base name associated with an abstract base type. *)

let base = function
  | XSpec (SpecBaseAbstract (_, _, properties)) ->
      properties.aty_var
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* Extracting the [check] function for an abstract base type, specializing
   for a reference-implementation value [v], and projecting via [Code.print]
   to obtain a piece of code. *)

let check xspec v : PPrint.document =
  match xspec with
  | XSpec (SpecBaseAbstract (ty1, _, properties)) ->
      Code.print (properties.aty_check (unwrap ty1 v))
  | _ ->
      assert false

(* -------------------------------------------------------------------------- *)

(* [choose selector v] chooses the reference side or the candidate side of
   the value [v], which has type [rany], producing a value of type [any]. *)

type selector =
  | Reference
  | Candidate

let show selector =
  match selector with
  | Reference ->
      "reference"
  | Candidate ->
      "candidate"

let choose selector (RAny (spec, reference, candidate)) =
  match selector with
  | Reference ->
      Any (left spec, reference)
  | Candidate ->
      Any (right spec, candidate)

(* -------------------------------------------------------------------------- *)

(* The functor [NewConcreteType] extends the type [spec] with a new case. The
   new type is regarded as concrete: its representation [t] is the same in the
   reference implementation and in the candidate implementation. The new data
   constructor has type [(t, t) spec]. *)

module NewConcreteType (X : sig
  type t
  val name : string
  val print : t -> PPrint.document
  val equal : (t -> t -> bool) code option
end) = struct

  (* Extend the type [ty] with a new constructor. *)

  module T = NewTy(struct type t = X.t end)

  let default_equality : (X.t -> X.t -> bool) code =
    code (lazy (PPrint.string "(=)")) (=)

  let equal =
    match X.equal with
    | Some equal ->
        equal
    | None ->
        default_equality

  (* Construct a specification whose projections are both [T.ty]. *)

  let spec =
    let properties = {
      cty_name = X.name;
      cty_print = X.print;
      cty_generate = None;
      cty_equal = equal;
    } in
    SpecBaseConcrete (T.ty, properties)

end

(* -------------------------------------------------------------------------- *)

(* The functor [NewAbstractType] extends the type [spec] with a new case. The
   new type is regarded as abstract: its representation is [t1] in the
   reference implementation and [t2] in the candidate implementation. The new
   data constructor has type [(t1, t2) spec]. *)

module NewAbstractType (X : sig
  type t1
  type t2
  val var : string
  val name : string
  val check : t1 -> (t2 -> unit) code
end) = struct

  open X

  (* Extend the type [ty] with two new constructors, one for each side. *)

  module T1 = NewTy(struct type t = t1 end)
  module T2 = NewTy(struct type t = t2 end)

  (* Construct a specification whose projections are [T1.ty] and [T2.ty]. *)

  let spec =
    SpecBaseAbstract (T1.ty, T2.ty, {
      aty_var = var; aty_name = name; aty_check = check
    })

end

(* -------------------------------------------------------------------------- *)

(* [normalize op polarity order_zero projectable spec] checks that the
   specification [spec] is well-formed and normalizes it. A normalized
   specification contains no occurrences of [SpecGenerate]. If the
   specification is ill-formed, the exception [IllFormedSpec] is raised. It is
   important to note that, because of [SpecDependentArrow], this exception is
   not necessarily raised when [normalize] is invoked; it can also be raised
   later on, when the normalized specification is used. *)

(* The need to normalize (as opposed to just checking well-formedness) arises
   not only from the desire to eliminate [SpecGenerate], but also from the
   presence of [SpecDependentArrow], where the ability to delay checking
   depends on the fact that we allowed to transform the specification. *)

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

(* The flag [projectable] indicates that a projectable type is expected, i.e.,
   one whose projections are defined; this rules out dependent arrows. This is
   the case when we are in the left-hand side of [map]. *)

(* The flag [gen] indicates whether we have seen [SpecGenerate] or
   [SpecGenerateSimplified] on the way down. This suppresses the
   error that is triggered when we reach a negative concrete type
   and do not have a generator. *)

exception IllFormedSpec of (* operation, message: *) string * string

let ill_formed op format =
  Printf.ksprintf (fun s -> raise (IllFormedSpec (op, s))) format

let rec normalize
: type r c . string -> bool -> bool -> bool -> bool -> (r, c) spec -> (r, c) spec
= fun op polarity order_zero projectable gen spec ->
  match spec with

  | SpecBaseConcrete (_, properties) ->
      (* If the polarity is negative (input), then this concrete type
         should be equipped with a generator. *)
      if not polarity && properties.cty_generate = None && not gen then
        ill_formed op
          "The concrete type `%s` is used in a negative position,\n\
           therefore it must be equipped with a generator."
          properties.cty_name;
      spec

  | SpecBaseAbstract _ ->
      spec

  | SpecPair (first, second) ->
      (* Under a pair, we expect an order-0 type. *)
      let order_zero = true in
      let gen = false in
      let first = normalize op polarity order_zero projectable gen first
      and second = normalize op polarity order_zero projectable gen second in
      SpecPair (first, second)

  | SpecArrow (domain, codomain) ->
      if order_zero then
        ill_formed op
          "A function `^^>` cannot appear in the left-hand side of a function\n\
           or under a pair.";
      let gen = false in
      let domain =
        let polarity = not polarity
        and order_zero = true in
        normalize op polarity order_zero projectable gen domain
      and codomain = normalize op polarity order_zero projectable gen codomain in
      SpecArrow (domain, codomain)

  | SpecDependentArrow (domain, codomain) ->
      if order_zero then
        ill_formed op
          "A dependent function `^&>` cannot appear in the left-hand side\n\
           of a function or under a pair.";
      if projectable then
        ill_formed op
          "A dependent function `^&>` cannot appear in the left-hand side of `map`.";
      let gen = false in
      let domain = normalize op (not polarity) true projectable gen domain
      (* Here, normalization is deferred until the moment where an actual
         argument is provided. Therefore, it is also repeated, every time
         an actual argument is provided. An alternative approach might be
         to generate an actual argument once, just for the purposes of
         normalizing and checking the codomain. However, not every type
         supports generation (in an empty environment); abstract types
         do not. *)
      and codomain rv = normalize op polarity order_zero projectable gen (codomain rv) in
      SpecDependentArrow (domain, codomain)

  | SpecSubset (spec, p) ->
      (* TODO if [subset] cannot be used in positive position, say so *)
      let gen = false in
      let spec = normalize op polarity order_zero projectable gen spec in
      SpecSubset (spec, p)

  | SpecNondet spec ->
      if not polarity then
        ill_formed op
          "The constructor `nondet` cannot be used in a negative position.";
      let gen = false in
      let spec = normalize op polarity order_zero projectable gen spec in
      begin match spec with
      | SpecBaseConcrete _ ->
          SpecNondet spec
      | _ ->
          ill_formed op
            "The constructor `nondet` can be applied only to a concrete type."
      end

  | SpecMap (c, rv, cv, domain, codomain) ->
      if not polarity then
        ill_formed op
          "The constructor `map` cannot be used in a negative position.";
      (* [domain] must be projectable. *)
      let gen = false in
      let domain =
        let projectable = true in
        normalize op polarity order_zero projectable gen domain
      and codomain = normalize op polarity order_zero projectable gen codomain in
      SpecMap (c, rv, cv, domain, codomain)

  | SpecGenerate (spec, generate) ->
      let gen = true in
      let spec = normalize op polarity order_zero projectable gen spec in
      begin match spec with
      | SpecBaseConcrete (ty, properties) ->
          let properties = { properties with cty_generate = Some generate } in
          SpecBaseConcrete (ty, properties)
      | _ ->
          ill_formed op
            "The constructor `&&@` can be applied only to a concrete type."
      end

  | SpecGenerateSimplified (spec, generate) ->
      let gen = true in
      let spec = normalize op polarity order_zero projectable gen spec in
      begin match spec with
      | SpecBaseConcrete (ty, properties) ->
          let generate () =
            let value = generate() in
            let ocaml = lazy (properties.cty_print value) in
            code ocaml value
          in
          let properties = { properties with cty_generate = Some generate } in
          SpecBaseConcrete (ty, properties)
      | _ ->
          ill_formed op
            "The constructor `&&&` can be applied only to a concrete type."
      end

let normalize op spec =
  let polarity = true
  and order_zero = false
  and projectable = false
  and gen = false in
  normalize op polarity order_zero projectable gen spec

let normalize_op (op, RAny (spec, rv, cv)) =
  (op, RAny (normalize op spec, rv, cv))
