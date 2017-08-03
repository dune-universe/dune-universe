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

(** Pretty-printing functions for BNF-grammars *)

open Format
open Bnf_spec
open Bnf
open Spec

val pp_prod : formatter -> symbol list -> unit
(** [pp_prod ppf syms] prettyprint symbols list [syms] using prettyprinter
    [ppf]. *)

val pp_live_prods : formatter -> int ProdMap.t -> unit
(** [pp_live_prods ppf syms] prettyprint live production map [pm] using
    prettyprinter [ppf]. *)

val pp_nt : formatter -> string -> ProdSet.t -> unit
(** [pp_nt ppf nt ps] prettyprint nonterminal [nt] and its production set
    [ps] using prettyprinter [ppf]. *)

val pp_live_nt : formatter -> string -> int * int ProdMap.t -> unit
(** [pp_nt ppf nt di] prettyprint live nonterminal [nt] and its derivation
    information [di] using prettyprinter [ppf]. *)

val pp_nt_map : formatter -> ProdSet.t NTMap.t -> unit
(** [pp_nt_map ppf nts] prettyprint map of nonterminals [nts] using
    prettyprinter [ppf]. *)

val pp_live_nts : formatter -> (int * int ProdMap.t) NTMap.t -> unit
(** [pp_live_nts ppf nt_di] prettyprint map of nonterminal derivation
    information [nt_di] using prettyprinter [ppf]. *)

val pp_ts : formatter -> TSet.t -> unit
(** [pp_ts ppf ts] prettyprint set of terminals [ts] using prettyprinter
    [ppf]. *)

val pp_nts : formatter -> NTSet.t -> unit
(** [pp_nts ppf nts] prettyprint set of nonterminals [nts] using
    prettyprinter [ppf]. *)

val pp_prods : formatter -> ProdSet.t -> unit
(** [pp_prods ppf prods] prettyprint set of productions [prods] using
    prettyprinter [ppf]. *)
