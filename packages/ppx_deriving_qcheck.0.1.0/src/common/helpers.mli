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

(** Helpers functions *)

open Ppxlib

val mk_loc : ?loc:location -> 'a -> 'a loc

(** [opt f] evaluates [f], if an exception is raised returns None *)
val opt : (unit -> 'a) -> 'a option

module Info : sig
  (** Information attached to a structure_item inside a record *)
  type t

  (** Info builder *)
  val create_info :
    ?name:string -> ?attr:attribute -> ?loc:location -> unit -> t

  (** Update name in info *)
  val update_name : string -> t -> t

  val get_loc : t -> location

  val get_name : t -> string

  val get_attribute : t -> attribute option
end

module Pairs : sig
  type 'a nested_pairs =
    | Pair of 'a nested_pairs * 'a nested_pairs
    | Double of 'a * 'a
    | Simple of 'a

  (** Takes a list of expression and nest them into pairs, in order
    to be used with [QCheck.pair] combinator 

    example:
   nest_generators [a] => a
   nest_generators [a;b] => Double a b
   nest_generators [a;b;c] => Pair (Simple a) (Double b c)
   nest_generators [a;b;c;d] => Pair (Double a b) (Double c d) *)
  val nest_generators : expression list -> expression nested_pairs

  (** Transforms nested pairs of expressions into an expression using
    [QCheck.pair combinator] *)
  val nested_pairs_to_expr : location -> expression nested_pairs -> expression

  (** Transforms nested pairs of 'a into a list of 'a, roundtrip with [nest_generators] *)
  val nested_pairs_to_list : 'a nested_pairs -> 'a list

  (** [names_from_gens f gens] creates unique name for the [gens]

      The name is created using [f] applied on the fresh integer identifier *)
  val names_from_gens :
    (string -> string) -> 'a nested_pairs -> string nested_pairs

  (** [patterns_from_gens loc f gens] creates a pattern from [gens],
    it also returns the generators names from [names_from_gens f gens] in order
    to be used afterward *)
  val pattern_from_gens :
    location ->
    (string -> string) ->
    'a nested_pairs ->
    pattern * string nested_pairs
end
