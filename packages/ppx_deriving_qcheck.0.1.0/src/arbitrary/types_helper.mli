(*****************************************************************************)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Valentin Chaboche                                      *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** Module helping with OCaml types as QCheck.arbitrary *)

open Ppxlib

(** Convention generator name for any type name *)
val name : string -> string

(** Module representing OCaml primitives types supported *)
module Primitive : sig
  (** [from_string loc recursives_types mutual_types s] tranforms [s] into
      a QCheck.arbitrary

      If [s] is present inside [recursives_types]
        -> arb_s (n - 1)
      Else if [s] is present inside [mutual_types]
        -> arb_s ()
      Else
        -> arb_s *)
  val from_string :
    loc:location ->
    recursives_types:string list ->
    mutual_types:string list ->
    string ->
    expression
end

(** [constr_type loc f args ()] transforms an applicated parametrizable type
    into a QCheck arbitrary.

    In that example, string is a parametrizable type applied on list 
    {[
    type t = string list [@@gen]

    let gen_t = QCheck.list QCheck.string
    ]}
*)
val constr_type :
  loc:location -> f:expression -> args:expression list -> unit -> expression

(** [from_longident loc recursives_types mutual_types lg] transforms [lg]
    into a QCheck.arbitrary

    Multiples cases:
    - The type is a identifier we use {!from_string}
    - The type comes from an outside module, we require a generator inside that
    outside module
    - The type is an application, we raise an exception *)
val from_longident :
  loc:location ->
  recursives_types:string list ->
  mutual_types:string list ->
  longident ->
  expression

(** Transform list of generators into a triple:
    
    - expressions nested with QCheck.pair expression
    - generators names used in the expression
    - pattern according to the nested expression *)
val nest_gens :
  loc:location -> expression list -> expression * string list * pattern

(** [record' loc gens label_decls] is an auxiliar function of {!record}.
    It extracts the pattern and according expression for [gens], also extracts
    the record's construction expression.
    
    type t = {left : int ; right = string }
    
    record' [QCheck.int; QCheck.string] [ left -> int ; right -> string ] =>
      - pattern : (arb_0, arb_1)
      - generators : pair QCheck.int QCheck.string
      - expression : { left = arb_0 ; right = arb_1 } *)
val record' :
  loc:location ->
  gens:expression list ->
  label_declaration list ->
  pattern * expression * expression

(** [record loc gens label_decls] convert [gens] and [label_decls] in a application
    of a record type using {!QCheck.map}.

    Example:
    {[
    type t = { left : int ; right : string } [@@gen]

    let gen_t =
      QCheck.map (fun (x,y) -> { left = x ; right = y })
      (QCheck.pair QCheck.int QCheck.string)
    ]}
 *)
val record :
  loc:location -> gens:expression list -> label_declaration list -> expression

(** [tuple' loc arbs ] is a {!tuple} auxiliar function, it extracts the pattern
    for a constructor application, the list of generators needed and the record expression
    
    type t = int * string
    
    tuple' [int; string] =>
      - pattern : (arb_0, arb_1)
      - generators : pair QCheck.int QCheck.string
      - expression : (arb_0 * arb_1) *)
val tuple' :
  loc:location -> expression list -> pattern * expression * expression

(** [tuple loc arbs] converts [arbs] in a application of a tuple

    Example:
    {[
    type t = int * int * int

    let gen_t =
      QCheck.map (fun (x,y,z) -> (x,y,z))
      (QCheck.triple int int int)
    ]} *)
val tuple : loc:location -> expression list -> expression

(** [constructors loc xs] convert a list of (weight option * constructor) into a single
    expression choosing the constructor using it's weight (1 if it's not provided).

    Example:
    {[
    type t =
    | A [@weight 5]
    | B [@weight 6]
    | C
    [@@gen]

    let gen_t =
      let open QCheck in
      frequency [ (5, always A) ;
                  (6, always B) ;
                  (1, always C) ]
    ]}
*)
val constructors :
  loc:location -> (expression option * expression) list -> expression

(**  TODO: ocamldoc that comment
     
    Convert a constructor name into an expression constructor QCheck.arbitrary

    Example:
    {[
    type t =
    | A

    (* A => QCheck.make @@ QCheck.Gen.return A *)
    ]}

    An additional case is supported when constructor requires arguments
    
    Example:
    {[
    type t =
    | A of int * string
    | B of { left : int ; right : string }

    (* A => QCheck.map (fun (x,y) -> A (x,y)) QCheck.(pair int string)
       B => QCheck.map (fun (x,y) -> B { left = x ; right ; y }) QCheck.(pair int string) *)
    ]}
*)
val constructor :
  loc:location ->
  kname:string ->
  ?kargs:pattern * expression * expression ->
  unit ->
  expression

(** [tree' loc leaves nodes ()] is almost the same function as {!tree'}
    the only difference is that QCheck.frequency is already applied to leaves and
    nodes *)
val tree' :
  loc:location -> leaves:expression -> nodes:expression -> unit -> expression

(** [tree loc leaves nodes ()] transforms a tree type like into a recursive
    arbitrary expression

    The recursive arbitrary uses a fuel, we could imagine that in future work
    the fuel would be provided by the user.

    Example:
    {[
    type t = Tree | Node of int * leaf * leaf
    [@@deriving arb]

    let rec arb_tree fuel =
      let open QCheck in
      match fuel with
      | 0 -> frequency [(1, always Leaf)]
      | n ->
        frequency
          [
            (1, always Leaf) ;
            (1, map
              (fun (arb_0, (arb_1, arb_2)) -> Node (arb_0, arb_1, arb_2))
              (pair int
                 (pair (arb_tree (n - 1)) (arb_tree (n - 1)))))
          ]

    let arb_tree = arb_tree 5
    ]}
 *)
val tree :
  loc:location ->
  leaves:(expression option * expression) list ->
  nodes:(expression option * expression) list ->
  unit ->
  expression

(** [variants loc ty xs] create a QCheck.arbitrary using [xs] to produce
    Ptyp_variant _ list. We also require [ty] to constraint the expression
    to a {[ t QCheck.arbitrary ]} where {[ t ]} is the current type we
    are deriving.

    `RTag represents the direct declaration of a variant {[type t = [`A]]}
    `RInh represents an inheritage of another variant {[type t' = [`B | t]]}

    Each variant can have a specific weight, see {!constructors}. *)
val variants :
  loc:location ->
  ty:string ->
  [< `RTag of string * expression option * expression list
  | `RInh of expression option * expression ]
  list ->
  expression

(** [gen loc is_rec args ty body] create a QCheck.arbitrary for [ty]

    When [is_rec] is false we produce the following structure_item:
    {[ let arb_ty args = body ]}

    The specific case where [is_rec] is true, the generator is self_recursive
    and we produce the following expression

    {[
    let arb_ty () = arb_ty' 5
    and arb_ty' fuel =
      match fuel with
      | 0 -> (* leaves *)
      | n -> (* recursives calls *)
    and arb_ty = arb_ty ()
    ]}

    (TODO: I did not test self recursive type with parameters inside args)

    (TODO: name gen is misleading, for now we produce an arbitrary but
    this will be later on transformed into a Gen.t) *)
val gen :
  loc:location ->
  is_rec:bool ->
  args:pattern list ->
  ty:string ->
  expression ->
  structure_item

val gens :
  loc:location ->
  tys:string list ->
  gens:structure_item list ->
  unit ->
  structure_item

(** [observable loc x] create an QCheck.Observable.t if [x] is an observable *)
val observable : loc:location -> core_type -> expression

(** [fun_nary loc obs x] create an ('a Tuple.t -> 'b) fun_ arbitrary *)
val fun_nary : loc:location -> expression list -> expression -> expression
