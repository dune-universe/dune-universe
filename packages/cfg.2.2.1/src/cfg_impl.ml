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

open Cfg_intf

module Make (Spec_ : SPEC) : (CFG with module Spec = Spec_) = struct
  module MySet = Set.Make
  module MyMap = Map.Make

  module Spec = Spec_
  open Spec

  let compare_syms s1 s2 =
    match s1, s2 with
    | NT nt1, NT nt2 -> compare_nt nt1 nt2
    | T t1, T t2 -> compare_t t1 t2
    | NT _, T _ -> 1
    | T _, NT _ -> -1

  let rec compare_lists cf l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | _, [] -> 1
    | [], _ -> -1
    | h1::t1, h2::t2 ->
        let c = cf h1 h2 in
        if c = 0 then compare_lists cf t1 t2 else c

  let compare_prod (p1, sl1) (p2, sl2) =
    let c = compare_prod p1 p2 in
    if c = 0 then compare_lists compare_syms sl1 sl2 else c

  module TSet = MySet (struct type t = Spec.t let compare = compare_t end)
  module TMap = MyMap (struct type t = Spec.t let compare = compare_t end)
  module NTSet = MySet (struct type t = nt let compare = compare_nt end)
  module NTMap = MyMap (struct type t = nt let compare = compare_nt end)

  module ProdSet = MySet (struct type t = prod * symbol list
                                 let compare = compare_prod end)

  module ProdMap = MyMap (struct type t = prod * symbol list
                                 let compare = compare_prod end)

  type grammar = ProdSet.t NTMap.t
  type live_grammar = (int * int ProdMap.t) NTMap.t

  let empty = NTMap.empty

  let maybe_get_prods gr nt =
    try NTMap.find nt gr
    with Not_found -> ProdSet.empty

  let add_prod gr nt pr sl =
    if sl = [] then invalid_arg "Cfg.add_prod: symbol list empty!"
    else NTMap.add nt (ProdSet.add (pr, sl) (maybe_get_prods gr nt)) gr

  let remove_nt gr nt = NTMap.remove nt gr

  let union_aux k v acc =
    try NTMap.add k (ProdSet.union (NTMap.find k acc) v) acc
    with Not_found -> NTMap.add k v acc

  let union gr1 gr2 = NTMap.fold union_aux gr2 gr1

  let diff_aux k v acc =
    try
      let prods_diff = ProdSet.diff (NTMap.find k acc) v in
      if ProdSet.is_empty prods_diff then NTMap.remove k acc
      else NTMap.add k prods_diff acc
    with Not_found -> acc

  let diff gr1 gr2 = NTMap.fold diff_aux gr2 gr1

  let inter_aux k v (res, gr as acc) =
    try
      let new_gr = NTMap.remove k gr in
      let k_prods = NTMap.find k gr in
      let prods_inter = ProdSet.inter k_prods v in
      if ProdSet.is_empty prods_inter then res, new_gr
      else NTMap.add k prods_inter res, new_gr
    with Not_found -> acc

  let inter gr1 gr2 = fst (NTMap.fold inter_aux gr2 (NTMap.empty, gr1))


  (** PRUNE UNPRODUCTIVE *)

  let sym_derivable nts = function
    | NT nt -> NTMap.mem nt nts
    | _ -> true

  let prod_defined defined_nts (_, syms) =
    List.for_all (sym_derivable defined_nts) syms

  let remove_unproductive nt prods defined_nts =
    let productive_prods = ProdSet.filter (prod_defined defined_nts) prods in
    if ProdSet.is_empty productive_prods then NTMap.remove nt defined_nts
    else NTMap.add nt productive_prods defined_nts

  let rec prune_unproductive gr =
    let remaining_nts = NTMap.fold remove_unproductive gr gr in
    if NTMap.equal ProdSet.equal remaining_nts gr then gr
    else prune_unproductive remaining_nts


  (** PRUNE NONLIVE *)

  exception Found of int

  let rec calc_syms_deriv n live_nts = function
    | [] -> raise (Found n)
    | T _ :: syms -> calc_syms_deriv n live_nts syms
    | NT nt :: syms ->
        let new_d =
          try Some (max (NTMap.find nt live_nts + 1) n)
          with Not_found -> None in
        match new_d with
        | None -> ()
        | Some new_n -> calc_syms_deriv new_n live_nts syms

  let calc_prod_deriv live_nts (_, syms) = calc_syms_deriv 1 live_nts syms
  let calc_prods_deriv live_nts = ProdSet.iter (calc_prod_deriv live_nts)

  let coll_live_info old_live nt prods (live_nts, nonlive_nts as live_info) =
    try calc_prods_deriv old_live prods; live_info
    with Found n -> NTMap.add nt n live_nts, NTMap.remove nt nonlive_nts

  let rec split_live_info (live_nts, nonlive_nts as live_info) =
    let new_live_nts, new_nonlive_nts as new_live_info =
      NTMap.fold (coll_live_info live_nts) nonlive_nts live_info in
    if
      NTMap.equal (=) live_nts new_live_nts &&
      NTMap.equal ProdSet.equal nonlive_nts new_nonlive_nts
    then live_info
    else split_live_info new_live_info

  let deriv_prods live_nts (_, syms as prod) prods =
    try calc_syms_deriv 1 live_nts syms; prods
    with Found n -> ProdMap.add prod n prods

  let derive_nts gr live_nts nt n =
    let prods =
      ProdSet.fold (deriv_prods live_nts) (NTMap.find nt gr) ProdMap.empty in
    NTMap.add nt (n, prods)

  let prune_nonlive gr =
    let live_nts, _ = split_live_info (NTMap.empty, gr) in
    NTMap.fold (derive_nts gr live_nts) live_nts NTMap.empty


  (** PRUNE UNREACHABLE *)

  let coll_reachable_sym gr acc = function
    | T _ -> acc
    | NT nt ->
        try NTMap.add nt (NTMap.find nt gr) acc
        with Not_found -> acc

  let coll_reachable_prod gr (_, syms) acc =
    List.fold_left (coll_reachable_sym gr) acc syms

  let coll_reachable_prods gr prods =
    ProdSet.fold (coll_reachable_prod gr) prods NTMap.empty

  let coll_reachable_nt gr _ prods =
    NTMap.fold NTMap.add (coll_reachable_prods gr prods)

  let rec get_unreachable gr root_nts =
    if NTMap.is_empty root_nts then gr
    else
      let new_gr = NTMap.fold (fun k _ -> NTMap.remove k) root_nts gr in
      let reachable_nts =
        NTMap.fold (coll_reachable_nt new_gr) root_nts NTMap.empty in
      get_unreachable new_gr reachable_nts

  let prune_unreachable gr start_sym =
    let s_prods = NTMap.find start_sym gr in
    let no_s_gr = NTMap.remove start_sym gr in
    diff gr (get_unreachable no_s_gr (coll_reachable_prods no_s_gr s_prods))


  (** CONVERSION FUNCTIONS *)

  let unlive_prod pr _ = ProdSet.add pr
  let unlive_prods prods = ProdMap.fold unlive_prod prods ProdSet.empty
  let unlive_nts nt (_, prods) = NTMap.add nt (unlive_prods prods)
  let grammar_of_live gr = NTMap.fold unlive_nts gr NTMap.empty

  let prune_live_prods gr nt prod _ acc =
    if ProdSet.mem prod (NTMap.find nt gr) then acc
    else ProdMap.remove prod acc

  let prune_live_nts gr nt (d, dprods) =
    try NTMap.add nt (d, ProdMap.fold (prune_live_prods gr nt) dprods dprods)
    with Not_found -> NTMap.remove nt

  let prune_unreachable_live live_gr start_sym =
    let gr = prune_unreachable (grammar_of_live live_gr) start_sym in
    NTMap.fold (prune_live_nts gr) live_gr live_gr

  let make_live gr = prune_nonlive (prune_unproductive gr)
  let make_sane gr = prune_unreachable (grammar_of_live (make_live gr))
  let make_sane_live gr = prune_unreachable_live (make_live gr)

  let grammar_contents gr = gr
  let deriv_depth_info live_gr = live_gr

  let nts_in_sym acc = function NT nt -> NTSet.add nt acc | _ -> acc
  let nts_in_prod (_, syms) acc = List.fold_left nts_in_sym acc syms
  let nts_in_nt nt prods acc = ProdSet.fold nts_in_prod prods (NTSet.add nt acc)
  let nts_in_grammar gr = NTMap.fold nts_in_nt gr NTSet.empty

  let ts_in_sym acc = function T t -> TSet.add t acc | _ -> acc
  let ts_in_prod (_, syms) acc = List.fold_left ts_in_sym acc syms
  let ts_in_nt _ = ProdSet.fold ts_in_prod
  let ts_in_grammar gr = NTMap.fold ts_in_nt gr TSet.empty

  let prods_in_nt _ = ProdSet.union
  let prods_in_grammar gr = NTMap.fold prods_in_nt gr ProdSet.empty


  (** BOUNDED DERIVATION *)

  let collect_sym (ts, nts) = function
    | T t -> TSet.add t ts, nts
    | NT nt -> ts, NTSet.add nt nts

  let collect_syms (_, syms) acc = List.fold_left collect_sym acc syms

  let bnd_descend_nt gr nt = ProdSet.fold collect_syms (NTMap.find nt gr)

  let rec bnd_descend levels gr reached_nts n =
    if n <= 0 || NTSet.is_empty reached_nts then levels
    else
      let _, new_reached_nts as this_level =
        NTSet.fold (bnd_descend_nt gr) reached_nts (TSet.empty, NTSet.empty) in
      bnd_descend (this_level :: levels) gr new_reached_nts (n - 1)

  let bnd_ascend_prod kept_nts (_, syms as prod) prods =
    if List.for_all (sym_derivable kept_nts) syms then prods
    else ProdSet.remove prod prods

  let bnd_ascend_nt gr kept_nts nt nts =
    let prods = NTMap.find nt gr in
    let kept_prods = ProdSet.fold (bnd_ascend_prod kept_nts) prods prods in
    if ProdSet.is_empty kept_prods then nts
    else NTMap.add nt kept_prods nts

  let cleanup_sym (ts, nts) =
    if TSet.is_empty ts && NTMap.is_empty nts then raise Exit
    else function
      | T t -> TSet.remove t ts, nts
      | NT nt -> ts, NTMap.remove nt nts

  let cleanup_prod (_, syms) level = List.fold_left cleanup_sym level syms

  let rec cleanup_levels kept_nts = function
    | [] -> assert false
    | (ts, nts as level)::rest as levels ->
        try
          let (bad_ts, bad_nts) =
            NTMap.fold (fun _ -> ProdSet.fold cleanup_prod) kept_nts level in
          let good_ts = TSet.diff ts bad_ts in
          let good_nts = NTMap.fold (fun nt _ -> NTMap.remove nt) bad_nts nts in
          if NTMap.is_empty bad_nts then
            if TSet.is_empty bad_ts then levels
            else (good_ts, nts) :: rest
          else (good_ts, good_nts) :: cleanup_levels good_nts rest
        with Exit -> levels

  let bnd_ascend gr (levels, kept_nts) (ts, nts) =
    let new_kept_nts = NTSet.fold (bnd_ascend_nt gr kept_nts) nts NTMap.empty in
    (ts, new_kept_nts) :: cleanup_levels new_kept_nts levels, new_kept_nts

  let bounded_grammar gr start n =
    match bnd_descend [] gr (NTSet.singleton start) n with
    | [] -> []
    | (ts, _last_nts) :: levels ->
        let init = [(ts, NTMap.empty)], NTMap.empty in
        fst (List.fold_left (bnd_ascend gr) init levels)
end
