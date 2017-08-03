(*
   CFG - Manipulation of Context-Free Grammars

   Copyright (C) 2000-2017  Markus Mottl
   email: markus.mottl@gmail.com
   WWW:   http://www.ocaml.info

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)


(** CFG - Library for the Manipulation of Context-Free Grammars *)

(** Specification of grammar entities *)
module type SPEC = sig
  (** Terminals *)
  type t

  (** Nonterminals *)
  type nt

  (** Productions *)
  type prod

  type symbol = NT of nt | T of t

  val compare_t : t -> t -> int
  val compare_nt : nt -> nt -> int
  val compare_prod : prod -> prod -> int
end

(** Interface to context-free grammars *)
module type CFG = sig
  (** Specification of grammar elements *)
  module Spec : SPEC
  open Spec

  module TSet : (Set.S with type elt = t)
  module TMap : (Map.S with type key = t)
  module NTSet : (Set.S with type elt = nt)
  module NTMap : (Map.S with type key = nt)
  module ProdSet : (Set.S with type elt = prod * symbol list)
  module ProdMap : (Map.S with type key = prod * symbol list)

  (** The type of context-free grammars *)
  type grammar

  (** The type of live CFGs *)
  type live_grammar

  val empty : grammar
  (** [empty] is the empty grammar. *)

  val add_prod : grammar -> nt -> prod -> symbol list -> grammar
  (** [add_prod gr nt prod sl] adds a production with tag [prod] that
      derives to symbol list [sl] to nonterminal [nt] in grammar [gr]. *)

  val remove_nt : grammar -> nt -> grammar
  (** [remove_nt gr nt] removes nonterminal [nt] from grammar [gr]. *)

  val union : grammar -> grammar -> grammar
  (** [union gr1 gr2] @return the union grammar of [g1] and [g2]. *)

  val diff : grammar -> grammar -> grammar
  (** [diff gr1 gr2] @return the difference grammar of [g1] and [g2]. *)

  val inter : grammar -> grammar -> grammar
  (** [inter gr1 gr2] @return the intersection grammar of [g1] and [g2]. *)

  val grammar_of_live : live_grammar -> grammar
  (** [grammar_of_live gr] converts a live grammar to a normal grammar. *)

  val prune_unproductive : grammar -> grammar
  (** [prune_unproductive gr] prunes all unproductive entitites in [gr]. *)

  val prune_nonlive : grammar -> live_grammar
  (** [prune_nonlive gr] prunes all nonlive entities in [gr]. *)

  val prune_unreachable : grammar -> nt -> grammar
  (** [prune_unreachable gr nt] prunes all entities in grammar [gr]
      which cannot be reached from nonterminal [nt].
      @raise Not_found if [nt] is not in [gr]. *)

  val prune_unreachable_live : live_grammar -> nt -> live_grammar
  (** [prune_unreachable_live gr nt] prunes all entities in live grammar
      [gr] which cannot be reached from nonterminal [nt]. The resulting
      grammar contains derivation information.
      @raise Not_found if [nt] is not in [gr]. *)

  val make_sane : grammar -> nt -> grammar
  (** [make_sane gr nt] prunes all useless entities in grammar [gr]
      using nonterminal [nt] as start symbol.
      @raise Not_found if [nt] is not in [gr]. *)

  val make_sane_live : grammar -> nt -> live_grammar
  (** [make_sane_live gr nt] prunes all useless entities in grammar [gr]
      using nonterminal [nt] as start symbol.
      @raise Not_found if [nt] is not in [gr]. *)

  val grammar_contents : grammar -> ProdSet.t NTMap.t
  (** [grammar_contents gr] returns a traversable representation of
      grammar [gr]. *)

  val deriv_depth_info : live_grammar -> (int * int ProdMap.t) NTMap.t
  (** [deriv_depth_info gr] returns a traversable representation of
      live grammar [gr]: the left part of the tuple to which nonterminals
      are mapped tells the minimum derivation depth needed to completely
      derive the corresponding nonterminal, the right part contains a
      map of productions which are mapped to their minimum derivation
      depth. *)

  val nts_in_grammar : grammar -> NTSet.t
  (** [nts_in_grammar gr] returns the set of all nonterminals in [gr]. *)

  val ts_in_grammar : grammar -> TSet.t
  (** [ts_in_grammar gr] returns the set of all terminals in [gr]. *)

  val prods_in_grammar : grammar -> ProdSet.t
  (** [prods_in_grammar gr] returns the set of all productions in [gr]. *)

  val bounded_grammar : grammar -> nt -> int -> (TSet.t * grammar) list
  (** [bounded_grammar gr nt bound] computes a list of derivation levels
      from grammar [gr], starting at start symbol [nt] and up to
      [bound]. Each level contains a set of terminals and a partial
      grammar which belong into this level. *)
end
